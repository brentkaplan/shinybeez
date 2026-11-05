"""Re-render the PNG consumers of the beez hex family from brand/*.svg.

    python3 brand/export.py [--out brand/out]

Headless Google Chrome rasterises each SVG at its native target size (crisper than
downscaling one master). Output lands under <out>/<repo>/<path>; copy into place.
Requires: Google Chrome, Pillow, numpy.
"""
import argparse
import os
import re
import subprocess
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
HEX_W, HEX_H = 240, 277

# (svg, repo-relative destination, width); height follows the viewBox ratio
JOBS = [
    ("hex-shinybeez", "shinybeez/app/static/img/hex-shinybeez.png", 600),
    ("hex-shinybeez", "shinybeez/shinybeez-logo.png", 326),
    ("hex-beezdemand", "beezdemand/man/figures/hex-beezdemand.png", 1024),
    ("hex-beezdemand", "beezdemand/man/figures/logo.png", 480),
    ("hex-beezdiscounting", "beezdiscounting/hex-beezdiscounting.png", 1024),
    ("hex-beezdiscounting", "beezdiscounting/man/figures/logo.png", 480),
    ("hex-shinybeez", "codedbx-web/assets/hex-shinybeez.png", 1024),
    ("hex-beezdemand", "codedbx-web/assets/hex-beezdemand.png", 1024),
    ("hex-beezdiscounting", "codedbx-web/assets/hex-beezdiscounting.png", 1024),
]
WATERMARK = ("hex-shinybeez", "shinybeez/app/static/img/shinybeez-watermark-alpha.png", 816, 83)


def render(svg_name, out_png, w, h):
    svg = open(f"{HERE}/{svg_name}.svg").read()
    vw, vh = re.search(r'width="(\d+)" height="(\d+)"', svg).groups()
    big = svg.replace(f'width="{vw}" height="{vh}"', f'width="{w}" height="{h}"', 1)
    os.makedirs(os.path.dirname(out_png), exist_ok=True)
    html = out_png + ".html"
    open(html, "w").write(f'<html><body style="margin:0;background:transparent"><div style="width:{w}px;height:{h}px">{big}</div></body></html>')
    subprocess.run([CHROME, "--headless=new", "--disable-gpu", "--hide-scrollbars", "--default-background-color=00000000",
                    f"--window-size={w},{h}", f"--screenshot={out_png}", f"file://{html}"], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    os.remove(html)
    im = Image.open(out_png)
    if im.size != (w, h):
        sys.exit(f"{out_png}: got {im.size}, wanted {(w, h)}")
    return im


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=f"{HERE}/out")
    out = ap.parse_args().out
    for svg, rel, w in JOBS:
        render(svg, f"{out}/{rel}", w, round(w * HEX_H / HEX_W))
        print(rel)
    svg, rel, w, amax = WATERMARK
    wm = render(svg, f"{out}/{rel}", w, round(w * HEX_H / HEX_W))
    a = np.asarray(wm.convert("RGBA")).copy()
    a[..., 3] = np.round(a[..., 3].astype(float) * amax / 255).astype(np.uint8)
    Image.fromarray(a, "RGBA").save(f"{out}/{rel}")
    print(rel, "alpha max", a[..., 3].max())
    # bee mark + favicon
    mk = open(f"{HERE}/mark-bee.svg").read()
    vw, vh = map(int, re.search(r'width="(\d+)" height="(\d+)"', mk).groups())
    render("mark-bee", f"{out}/shinybeez/brand/mark-bee.png", 1024, round(1024 * vh / vw))
    fav = render("mark-bee", f"{out}/_favicon-192.png", 192, round(192 * vh / vw)).convert("RGBA")
    canvas = Image.new("RGBA", (192, 192), (0, 0, 0, 0))
    canvas.alpha_composite(fav, (0, (192 - fav.height) // 2))
    os.makedirs(f"{out}/shinybeez/app/static", exist_ok=True)
    canvas.resize((48, 48), Image.LANCZOS).save(f"{out}/shinybeez/app/static/favicon.ico", format="ICO", sizes=[(48, 48)])
    print("shinybeez/app/static/favicon.ico")


if __name__ == "__main__":
    main()
