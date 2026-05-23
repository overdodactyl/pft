# ATS classification label fix — for clinical review

**Status:** Implemented as a bug fix on master. This memo summarizes the change for clinical-team review.

## Background

`pft::ats_classification()` assigns one of 5 pattern labels — Normal, Non-specific, Obstructed, Restricted, Mixed — to a row of spirometry + lung-volume measurements by comparing FEV1, FVC, FEV1/FVC, and TLC against their respective LLNs.

The function evaluates all 16 combinations of the 4 inputs (each normal `N` or abnormal `A`), labelling each combination explicitly. Two of the 16 cases were assigned the wrong label, and a separate typo in the "all normal" branch was compared against the wrong LLN.

## Defects fixed

### 1. Inverted labelling of ANNN and NANN

| Combination | Inputs (FEV1, FVC, FEV1/FVC, TLC) | Old label | New label |
|---|---|---|---|
| `ANNN` | FEV1↓, FVC normal, FEV1/FVC normal, TLC normal | Non-specific | **Normal** |
| `NANN` | FEV1 normal, **FVC↓**, FEV1/FVC normal, TLC normal | Normal | **Non-specific** |

Per **Stanojevic et al. (2022)** — *ERS/ATS technical standard on interpretive strategies for routine lung function tests*, Eur Respir J 60(1):2101499, doi:[10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021):

- **Figure 8** (spirometry interpretation flowchart): the decision tree starts with FEV1/FVC, then branches on FVC, then TLC. FEV1 is not part of the pattern-classification path — it is used for severity grading only. With FEV1/FVC normal and FVC normal, the algorithm terminates at "Spirometry normal" regardless of FEV1.
- **Table 5** (pattern definitions): the Non-specific pattern is defined as `FEV1↓ + FVC↓ + FEV1/FVC normal + TLC normal`. The crucial element is **reduced FVC**; an isolated low FEV1 with normal FVC, FEV1/FVC, and TLC is not in the table.

The pre-existing 5-branch implementation (initial commit) did not cover `ANNN` or `NANN` at all — these labels were freshly assigned when the function was expanded to handle all 16 combinations (commit `30d7271`, "feat(ats-classification): expand to 16 TLC-aware pattern combinations"). The FEV1 and FVC priorities appear to have been transposed in the two corresponding new branches.

### 2. FVC compared against the wrong LLN in the all-normal branch

In the NNNN branch (line 49 of `R/ats_classification.R`), FVC was being compared against `fev1_lln` instead of `fvc_lln`:

```r
# before
} else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fev1_lln[i]) && ... ) {
# after
} else if ( (fev1[i] >= fev1_lln[i]) && (fvc[i] >= fvc_lln[i]) && ... ) {
```

This typo pre-dates the 16-combo expansion (present in the 5-branch original). For patients with FVC between FEV1's LLN and FVC's LLN, FVC would erroneously satisfy the Normal branch — bypassing the `NANN` check entirely and labelling the patient "Normal" even though they have a clinically reduced FVC.

## Who is affected

Both defects only affect patients with **FEV1/FVC normal and TLC normal**. The Obstructed / Restricted / Mixed pathways are unchanged.

Within that population, the change re-labels patients whose spirometry profile is one of:

- **FEV1 below LLN, FVC ≥ LLN (i.e. genuinely normal):** old label "Non-specific" → new label "Normal".
- **FVC below LLN (FEV1 may be normal or low):** old label "Normal" or "Non-specific" depending on FEV1 — both now consistently "Non-specific".

In our local PFT data the proportion affected is expected to be small (isolated low FEV1 with normal FVC and ratio is unusual; isolated low FVC with normal FEV1 is also uncommon), but the relabelling is clinically meaningful when it does occur — "Non-specific" carries a different downstream interpretation than "Normal."

## Algorithm now followed

The corrected branch table maps exactly to Stanojevic 2022 Figure 8 for all combinations the figure addresses, and uses a defensible default (Restricted) for the residual combinations where TLC<LLN but the figure's algorithm wouldn't reach a TLC check (e.g. `NNNA`, `ANNA`). Specifically:

| `combo` | Inputs | New label | Source in Stanojevic 2022 |
|---|---|---|---|
| NNNN | all normal | Normal | Figure 8 ("Spirometry normal") |
| ANNN | only FEV1↓ | Normal | Figure 8 (FVC normal terminates at "Normal"; FEV1↓ alone is not a pattern) |
| NANN | only FVC↓ | Non-specific | Figure 8 (FVC↓, TLC normal → "Non-specific") |
| AANN | FEV1↓ + FVC↓ | Non-specific | Table 5 (Non-specific definition) |
| NNAN / ANAN / NAAN / AAAN | FEV1/FVC↓ + TLC normal | Obstructed | Figure 8 (obstruction branch, TLC normal) |
| NNNA / ANNA / NANA / AANA | TLC↓, FEV1/FVC normal | Restricted | Figure 8 / Table 5 (TLC↓ with normal ratio); rows where FVC is normal are extrapolations beyond the paper |
| NNAA / ANAA / NAAA / AAAA | FEV1/FVC↓ + TLC↓ | Mixed | Figure 8 (obstruction + restriction); Table 8 ("Mixed: FEV1/FVC and TLC both <5th percentile") |

## Questions for clinical review

1. **`ANNN` → Normal** (isolated low FEV1 with everything else normal): Is treating this as Normal acceptable, given that the patient does have an abnormal value? Stanojevic 2022 says it is — FEV1 is severity-grading, not pattern-classifying — but clinical practice may want a "review/repeat" flag.
2. **`NNNA` and related TLC-only abnormalities**: Stanojevic 2022's algorithm doesn't reach these combinations (TLC is only checked after FVC is found low). We default to "Restricted" when TLC<LLN even if FVC is normal. Acceptable, or should we add a "TLC abnormal — needs evaluation" label?
3. **No Dysanapsis label**: Stanojevic 2022 Table 5 lists Dysanapsis as a separate pattern (FEV1 normal, FVC normal/↑, FEV1/FVC↓). Currently we fold this into "Obstructed" via the `NNAN` branch. Acceptable simplification, or do we want to add it?

## Citation

Primary algorithm reference now cited in `R/ats_classification.R` via `@references`:

- Stanojevic S, Kaminsky DA, Miller MR, et al. ERS/ATS technical standard on interpretive strategies for routine lung function tests. *Eur Respir J*. 2022;60(1):2101499. doi:[10.1183/13993003.01499-2021](https://doi.org/10.1183/13993003.01499-2021).
- Pellegrino R, Viegi G, Brusasco V, et al. Interpretative strategies for lung function tests. *Eur Respir J*. 2005;26(5):948-968. doi:[10.1183/09031936.05.00035205](https://doi.org/10.1183/09031936.05.00035205) — predecessor standard.
