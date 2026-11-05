import vtracer, sys, os
S=sys.argv[1]
A=f"{S}/assets"
# hi-fi: keep gradients (many color layers); clean: posterized few colors
cfgs={
 "hifi": dict(colormode="color", hierarchical="stacked", mode="spline", filter_speckle=8, color_precision=7, layer_difference=12, corner_threshold=60, length_threshold=4.0, max_iterations=10, splice_threshold=45, path_precision=3),
 "clean": dict(colormode="color", hierarchical="stacked", mode="spline", filter_speckle=24, color_precision=5, layer_difference=40, corner_threshold=70, length_threshold=6.0, max_iterations=10, splice_threshold=45, path_precision=2),
}
for name,cfg in cfgs.items():
    vtracer.convert_image_to_svg_py(f"{A}/bee.png", f"{A}/bee-{name}.svg", **cfg)
    print(name, os.path.getsize(f"{A}/bee-{name}.svg"))
for c in ["demand-curve","discount-curve"]:
    vtracer.convert_image_to_svg_py(f"{A}/{c}.png", f"{A}/{c}.svg", colormode="color", hierarchical="stacked", mode="spline", filter_speckle=10, color_precision=6, layer_difference=24, corner_threshold=60, length_threshold=4.0, path_precision=3)
    print(c, os.path.getsize(f"{A}/{c}.svg"))
