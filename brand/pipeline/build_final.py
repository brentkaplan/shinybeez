"""Final hex-logo build: A3-fixed geometry with the touched-up assets and the
wordmark emitted as Futura Medium glyph outlines (no font dependency).
usage: build_final.py SCRATCH OUTDIR
Writes OUTDIR/hex-<name>.svg, OUTDIR/mark-bee.svg and an .html wrapper per SVG
for headless-Chrome rasterising (render.sh)."""
import json
import math
import os
import re
import sys

from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTCollection

S, OUT = sys.argv[1], sys.argv[2]
A = f"{S}/assets"
os.makedirs(OUT, exist_ok=True)
rects = json.load(open(f"{A}/rects.json"))
SQ3 = math.sqrt(3); VBW, VBH, CX, CY, R = 240, 277, 120, 138.5, 132
STROKE = 5.0
YB_BY_NAME = {"beezdemand": 218, "beezdiscounting": 218, "shinybeez": 222}
FRAME_RECT = [10.0, 11.5, 220.0, 254.0]
INK, CREAM = "#141414", "#f4ecd4"

# ---------------------------------------------------------------- font
FONT = TTCollection("/System/Library/Fonts/Supplemental/Futura.ttc").fonts[0]   # Futura Medium
UPM = FONT["head"].unitsPerEm
CMAP = FONT.getBestCmap()
GLYPHS = FONT.getGlyphSet()
HMTX = FONT["hmtx"]
KERN = {}
if "kern" in FONT:
    for sub in FONT["kern"].kernTables:
        KERN.update(getattr(sub, "kernTable", {}))


def layout(label):
    """[(glyph name, x offset in font units)], total advance."""
    names = [CMAP[ord(ch)] for ch in label]
    x, out = 0, []
    for i, g in enumerate(names):
        if i:
            x += KERN.get((names[i - 1], g), 0)
        out.append((g, x))
        x += HMTX[g][0]
    return out, x


def word_path(label, fs, cx, baseline):
    """One SVG path (d attribute) for the whole label, centred on cx, in viewBox units."""
    items, adv = layout(label)
    k = fs / UPM
    x0 = cx - adv * k / 2
    d = []
    for g, x in items:
        pen = SVGPathPen(GLYPHS)
        # font y is up, SVG y is down: scale (k, -k), translate to the glyph origin
        tpen = TransformPen(pen, (k, 0, 0, -k, x0 + x * k, baseline))
        GLYPHS[g].draw(tpen)
        d.append(pen.getCommands())
    return " ".join(d)


def fitword(label, yb, margin=4.0, descent=4.0):
    Ri = R - 5
    y_low = yb + descent
    hw = (SQ3 / 2 * Ri) * (CY + Ri - y_low) / (Ri / 2) - STROKE / 2
    avail = 2 * hw - 2 * margin
    _, adv = layout(label)
    return min(20, avail / (adv / UPM))


# ---------------------------------------------------------------- geometry
def hexpts(r):
    dx = SQ3 / 2 * r
    return " ".join(f"{x:.2f},{y:.2f}" for x, y in [(CX, CY - r), (CX + dx, CY - r / 2), (CX + dx, CY + r / 2), (CX, CY + r), (CX - dx, CY + r / 2), (CX - dx, CY - r / 2)])


def svg_group(path, rect, gid):
    txt = open(path).read()
    W = int(re.search(r'width="(\d+)"', txt).group(1)); H = int(re.search(r'height="(\d+)"', txt).group(1))
    body = "".join(re.findall(r'<path[^>]*/>|<path.*?</path>', txt, flags=re.S))
    x, y, w, h = rect
    return f'<g id="{gid}" transform="translate({x},{y}) scale({w/W:.6f},{h/H:.6f})">{body}</g>'


def hexsvg(name, motifs):
    yb = YB_BY_NAME[name]
    label = "{" + name + "}"
    fs = fitword(label, yb)
    parts = [svg_group(f"{A}/{m}-curve-final.svg", rects["demand" if m == "demand" else "discount"], f"curve-{m}") for m in motifs]
    parts.append(svg_group(f"{A}/asterisks.svg", FRAME_RECT, "asterisks"))
    parts.append(svg_group(f"{A}/bee-final.svg", rects["bee"], "bee"))
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {VBW} {VBH}" width="{VBW}" height="{VBH}" role="img" aria-label="{label} hex logo">
<title>{label}</title>
<defs><clipPath id="hex-clip"><polygon points="{hexpts(R-5)}"/></clipPath></defs>
<polygon id="interior" fill="{CREAM}" points="{hexpts(R-5)}"/>
<g clip-path="url(#hex-clip)">{"".join(parts)}</g>
<polygon id="frame" fill="none" stroke="{INK}" stroke-width="{STROKE}" stroke-linejoin="round" points="{hexpts(R-5)}"/>
<path id="wordmark" fill="{INK}" d="{word_path(label, fs, CX, yb)}"/>
</svg>
''', fs


def write(name, svg, w, h):
    open(f"{OUT}/{name}.svg", "w").write(svg)
    big = svg.replace(f'width="{VBW}" height="{VBH}"', f'width="{w}" height="{h}"', 1)
    open(f"{OUT}/{name}.html", "w").write(f'<html><body style="margin:0;background:transparent"><div style="width:{w}px;height:{h}px">{big}</div></body></html>')


LOGOS = {"beezdemand": ["demand"], "beezdiscounting": ["discount"], "shinybeez": ["demand", "discount"]}
sizes = {}
for name, motifs in LOGOS.items():
    svg, fs = hexsvg(name, motifs)
    write(f"hex-{name}", svg, 2048, 2363)
    sizes[name] = round(fs, 1)
x, y, w, h = rects["bee"]
mark = f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="{x-4:.2f} {y-4:.2f} {w+8:.2f} {h+8:.2f}" width="{w+8:.0f}" height="{h+8:.0f}" role="img" aria-label="beez bee mark"><title>beez</title>{svg_group(f"{A}/bee-final.svg", rects["bee"], "bee")}</svg>\n'
open(f"{OUT}/mark-bee.svg", "w").write(mark)
mh = round(1024 * (h + 8) / (w + 8))
open(f"{OUT}/mark-bee.html", "w").write('<html><body style="margin:0;background:transparent"><div style="width:1024px;height:%dpx">%s</div></body></html>' % (mh, mark.replace(f'width="{w+8:.0f}" height="{h+8:.0f}"', f'width="1024" height="{mh}"', 1)))
print("final SVGs written:", sizes, "kern pairs:", len(KERN))
