# CRAN submission walkthrough for pft 1.0.0

## Prerequisites (should already be true, verify anyway)

- `DESCRIPTION` has `Version: 1.0.0` (no dev suffix)
- `NEWS.md` has an entry for `# pft 1.0.0`
- `cran-comments.md` exists at repo root with the test-environment blurb
- `pft_1.0.0.tar.gz` exists (I built it for you; if you're on a different
  machine, run `R CMD build .` first)
- `R CMD check --as-cran pft_1.0.0.tar.gz` locally passes with 0 errors,
  0 real warnings, 0 real notes

## Option A: `devtools::submit_cran()` from R (recommended)

This is the automated path. In a fresh R session at the repo root:

```r
library(devtools)
submit_cran()
```

### What you'll see and how to answer

1. **`devtools` builds the package** (if `pft_1.0.0.tar.gz` doesn't already
   exist in the parent dir, it will run `R CMD build .` for you).

2. **`devtools` prints the release checklist** and asks:
   > `Is DESCRIPTION up-to-date?`
   Answer: `yes` (we just updated it).
   > `Have you checked on R-hub with rhub::rhub_check()?`
   Answer: `yes` (we did — all 5 platforms passed).
   > `Have you updated NEWS.md?`
   Answer: `yes`.

3. **`devtools` shows the DESCRIPTION** and asks:
   > `Is this correct?`
   Answer: `yes` if everything reads right.

4. **`devtools` shows the cran-comments.md content** and asks:
   > `Ready to submit?`
   Answer: `yes` to send.

5. **CRAN sends a confirmation email to `johnson.pat@mayo.edu`** within
   ~5 minutes. Subject line will be
   `[CRAN-pretest] Package pft 1.0.0`.

6. **Click the confirmation link in that email.** This is required — the
   submission does NOT proceed until you click.

7. After you click, CRAN's automated pretest runs (~1-24 hours depending
   on queue). You'll get a second email with the pretest results:
   - **Success**: package moves to manual review (usually 1-5 business days).
   - **Failure with NOTEs/WARNINGs**: CRAN's email lists what to fix; make
     the changes, bump the version to 1.0.1, and resubmit.

## Option B: Web upload (if devtools breaks for any reason)

1. Go to https://cran.r-project.org/submit.html
2. Fill in:
   - Your name: Patrick W. Johnson
   - Your email: johnson.pat@mayo.edu (must match Maintainer in DESCRIPTION)
   - Package: attach `pft_1.0.0.tar.gz`
   - Solicitation from CRAN: (leave blank)
   - Additional comments: paste the contents of `cran-comments.md`
3. Submit. Same email-confirmation loop as option A.

## After acceptance

CRAN will send a "package accepted" email. At that point:

1. **Tag doesn't need re-doing** — v1.0.0 is already tagged.
2. **Update SoftwareX paper** if CRAN accepted a version different from
   1.0.0 (if you had to bump to 1.0.1 for a fix). Update C1, C2, S1 rows.
3. **NEWS.md**: add `# pft (development version)` heading above the
   `# pft 1.0.0` heading so future changes have a place to go.
4. **DESCRIPTION**: bump to `1.0.0.9000` (or `1.0.1.9000`) to indicate
   post-release development.

## Common CRAN pretest feedback (be ready)

Most first-time submissions get 1-2 minor requests. The most common:

- **"Please add small executable examples"** — if any function has only
  `\dontrun{}` examples, wrap them in `\donttest{}` or `if(interactive())`
  instead. `\dontrun{}` is disliked; it should be reserved for examples
  that would actually break in the CRAN environment.
- **"Please add \\value tags"** — every exported function needs a
  `\value` section in its .Rd. We already verified all are present.
- **"Please write package name in single quotes"** — in the Description
  field, package names should be `'pft'` not just `pft`.
- **"Please remove commented-out code"** — none in our sources.
- **"Please use `tempdir()` in examples"** — we don't write files, so
  this shouldn't apply.

If you get feedback, reply to the CRAN email with:
1. A short list of what you changed (bullet form)
2. Confirm the new tarball passes R CMD check --as-cran locally
3. Attach the new tarball (or say you resubmitted via the portal)

## If CRAN accepts but the paper is still in review

That's fine. CRAN acceptance and SoftwareX submission are independent
timelines. Update the paper's C2 URL to point at CRAN
(`https://cran.r-project.org/package=pft`) once accepted; keep the
Zenodo DOI in S2 either way.

## Things NOT to do

- **Do not submit a version that Gauss's local check flagged as clean but
  no CI has actually verified.** We've already verified via GitHub Actions
  matrix + R-hub 5-platform pass, so we're OK here.
- **Do not resubmit within 24 hours of a rejection without changes.** CRAN
  maintainers explicitly ask for this.
- **Do not respond to the CRAN pretest email from a different address than
  the maintainer email.** They will not process it.
