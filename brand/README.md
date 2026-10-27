# beez brand assets

Source of truth for the hex-sticker family used by shinybeez, beezdemand, beezdiscounting and codedbx.com.

| File | What |
|---|---|
| `hex-shinybeez.svg`, `hex-beezdemand.svg`, `hex-beezdiscounting.svg` | Full hex stickers, 240×277 viewBox. All vector: the illustrated bee and the two fitted curves were traced from the original 5000 px artwork; the `{name}` wordmark is Futura Medium converted to outlines, so no font is needed to render them. |
| `mark-bee.svg`, `mark-bee.png` | Bee only, for places where a full hex is unreadable (favicon, 40 px navbar marks). |
| `export.py` | Re-renders every PNG consumer from these SVGs with headless Chrome. Run it after editing an SVG. |

## Where the rasters go

| Consumer | File | Size |
|---|---|---|
| shinybeez navbar + info modal | `app/static/img/hex-shinybeez.png` | 600×692 |
| shinybeez plot watermark | `app/static/img/shinybeez-watermark-alpha.png` | 816×943, alpha scaled to max 83 (opacity baked in, as before) |
| shinybeez favicon | `app/static/favicon.ico` | 48×48 from the bee mark |
| ShinyProxy landing page (raw URL on `main`) | `shinybeez-logo.png` | 326×376 |
| beezdemand README / pkgdown | `man/figures/hex-beezdemand.png` 1024×1182, `man/figures/logo.png` 480×554 |
| beezdiscounting README (raw URL on `master`) / pkgdown | `hex-beezdiscounting.png` 1024×1182, `man/figures/logo.png` 480×554 |
| codedbx.com software page | `assets/hex-*.png` 1024×1182 |

The package repos carry a copy of their own SVG under `man/figures/` as a generated snapshot; edit here, then re-export.

## Regenerating

```sh
python3 brand/export.py            # writes PNGs under brand/out/<repo>/... ; copy into place
```

Needs Google Chrome (headless rendering), Pillow and numpy. The trace-and-build pipeline that produced the SVGs (asset extraction from the Keynote masters, vtracer settings, knob/curve touch-ups, Futura outlining with fontTools) is kept outside the repo in `design-mockups/hex-hq/src/scripts/` (local, gitignored) together with every candidate that was considered.

Design notes: frame stroke 5 (2.2 % of hex width, as the 2024 original); wordmark baselines 218 (packages) and 222 (shinybeez, to clear the discounting curve); four `*` marks restored from the original; curves ~1.7× the original line weight so they survive 120 px.
