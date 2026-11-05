"""Print-level touch-ups on the traced assets, before the final build.
  1. Bee: replace the lumpy antenna/leg end-knobs with true circles (found as
     distance-transform peaks of the black mask that survive an erosion the
     line strokes do not), on the 2x raster, then retrace -> bee-final.svg
  2. Curves: rebuild the thickened line as a solid dark dilation of the line
     core (no nearest-colour fill, which left a pale ghost hairline), dots
     untouched -> {demand,discount}-curve-final.svg
usage: finish_assets.py SCRATCH"""
import sys

import numpy as np
import vtracer
from PIL import Image, ImageDraw
from scipy import ndimage

S = sys.argv[1]; A = f"{S}/assets"

# ---------------------------------------------------------------- 1. bee knobs
bee = Image.open(f"{A}/bee-2x.png").convert("RGBA")
arr = np.asarray(bee).astype(np.int16)
rgb, al = arr[..., :3], arr[..., 3]
black = (al > 128) & (rgb.sum(2) < 180)
dist = ndimage.distance_transform_edt(black)
# stroke half-width ~ median of dist along the skeleton-ish interior; knobs are much thicker
line_hw = np.percentile(dist[black & (dist > 1)], 60)
core = dist > line_hw * 1.55            # only knob centres survive
lbl, n = ndimage.label(core)
knobs = []
H, W = black.shape
yy, xx = np.mgrid[0:H, 0:W]
for i, sl in enumerate(ndimage.find_objects(lbl), start=1):
    m = lbl[sl] == i
    d = dist[sl] * m
    cy, cx = np.unravel_index(np.argmax(d), d.shape)
    r = d[cy, cx]
    x, y = sl[1].start + cx, sl[0].start + cy
    if not (line_hw * 2.2 < r < line_hw * 4.5):      # end-knobs are ~2.5-3.8x the stroke half-width
        continue
    # an end-knob has exactly one stroke leaving it: black coverage of the annulus r..1.6r is small
    rr = ((xx - x) ** 2 + (yy - y) ** 2) ** 0.5
    ann = (rr > r * 1.05) & (rr < r * 1.6)
    cov = black[ann].mean()
    disc = black[rr <= r * 0.95].mean()
    if cov < 0.30 and disc > 0.97:
        knobs.append((int(x), int(y), float(r)))
print(f"line half-width {line_hw:.1f}px; knob circles found: {len(knobs)}", [(x, y, round(r, 1)) for x, y, r in knobs])
# paint perfect circles (4x supersampled for a clean edge) over each knob
up = 4
big = bee.resize((bee.width * up, bee.height * up), Image.NEAREST)
d = ImageDraw.Draw(big)
for x, y, r in knobs:
    rr = r * up
    d.ellipse((x * up - rr, y * up - rr, x * up + rr, y * up + rr), fill=(20, 20, 20, 255))
fixed = big.resize(bee.size, Image.LANCZOS)
fixed.save(f"{A}/bee-2x-final.png")
vtracer.convert_image_to_svg_py(f"{A}/bee-2x-final.png", f"{A}/bee-final.svg", colormode="color", hierarchical="stacked",
                                mode="spline", filter_speckle=60, color_precision=6, layer_difference=36,
                                corner_threshold=90, length_threshold=10.0, max_iterations=12, splice_threshold=60, path_precision=2)

# ---------------------------------------------------------------- 2. curves
for c in ["demand-curve", "discount-curve"]:
    im = np.asarray(Image.open(f"{A}/{c}.png").convert("RGBA")).copy()
    a = im[..., 3] > 60
    dark = a & (im[..., :3].sum(2) < 240)           # the fitted line (dots are grey ~ 3*150)
    dots = ndimage.binary_opening(a & ~dark, iterations=4)          # drop the thin anti-alias halo, keep the grey discs
    line = ndimage.binary_dilation(dark & ~ndimage.binary_dilation(dots, iterations=2), iterations=6)   # ~1.7x the original line
    out = np.zeros_like(im)
    out[dots] = im[dots]
    out[line] = (34, 34, 34, 255)                    # solid line, drawn over the dots
    Image.fromarray(out, "RGBA").save(f"{A}/{c}-final.png")
    vtracer.convert_image_to_svg_py(f"{A}/{c}-final.png", f"{A}/{c}-final.svg", colormode="color", hierarchical="stacked",
                                    mode="spline", filter_speckle=10, color_precision=4, layer_difference=80,
                                    corner_threshold=60, length_threshold=4.0, path_precision=3)
print("final assets written")
