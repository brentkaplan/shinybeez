# How the brand SVGs were made (2026-10-26)

Run in order, each with `python -I <script> <scratch>` where `<scratch>` holds `src/` (the three
5000 px PNGs unzipped from `~/Downloads/beezfamily.key`, `Data/beez-logos1.00{1,2,3}-*.png`) and an
empty `assets/`. Venv: pillow, numpy, scipy, vtracer, fonttools. Chrome is used for rasterising.

1. `extract_full.py` — cream-key + connected-component extraction of the bee and the two fitted curves
   (with their data points) at native resolution; writes `assets/rects.json` (placements in the
   240×277 viewBox).
2. `trace.py` — first vtracer pass (hi-fi and clean settings); only the curve settings are reused.
3. `fix_assets.py` — restores the four `*` marks from the master, thickens the curves, retraces the bee
   from a 2× upsample.
4. `finish_assets.py` — print touch-ups: end-knobs replaced by true circles (distance-transform
   detection), curves rebuilt as a solid dark dilation, final traces `assets/*-final.svg`.
5. `build_final.py <scratch> <out>` — assembles the hex (frame 5, interior #f4ecd4, asterisks, curves,
   bee), outlines the `{name}` wordmark with Futura Medium via fontTools (kerning applied), writes
   `hex-*.svg` and `mark-bee.svg`. Baselines: 218 (packages), 222 (shinybeez).

Then `brand/export.py` renders the PNG set. The judging record (three reviewers, before/after) is in
`design-mockups/hex-hq/judge/` locally.
