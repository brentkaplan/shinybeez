import numpy as np, json, sys, vtracer
from PIL import Image
from scipy import ndimage
S=sys.argv[1]; A=f"{S}/assets"; SRC=f"{S}/src"
MX0,MX1,MY0,MY1 = 10.0,230.0,11.5,265.5
def load(p): return np.asarray(Image.open(p).convert("RGBA")).astype(np.int16)
def cream(arr):
    rgb,a=arr[...,:3],arr[...,3]; op=a>30
    return np.median(rgb[op&(rgb.mean(2)>195)&((rgb.max(2)-rgb.min(2))<38)],0)
def hexbb(arr):
    ys,xs=np.where(arr[...,3]>10); return xs.min(),ys.min(),xs.max(),ys.max()
def noncream(arr,cr):
    rgb,a=arr[...,:3],arr[...,3]; return (a>30)&(np.sqrt(((rgb-cr)**2).sum(2))>34)
ad=load(f"{SRC}/beez-logos1.002-408.png"); cd=cream(ad); hb=hexbb(ad)
nc=noncream(ad,cd); lbl,n=ndimage.label(nc,structure=np.ones((3,3)))
objs=ndimage.find_objects(lbl); area=ndimage.sum(np.ones_like(lbl),lbl,index=range(1,n+1))
# asterisks: the four `*` marks beside the bee (area 4000-9000 px, ~120 px square, mid-height),
# which excludes the eyes (~67x83) and the wordmark glyphs (y > 3500)
keep=[]
for i,s in enumerate(objs):
    h=s[0].stop-s[0].start; w=s[1].stop-s[1].start
    if 4000<area[i]<9000 and 100<w<140 and 100<h<130 and 2300<s[0].start<3000: keep.append(i+1)
print("asterisks kept:",len(keep)); assert len(keep)==4, keep
mask=np.isin(lbl,keep)
out=np.zeros((*mask.shape,4),np.uint8); out[...,:3]=20; out[...,3]=mask*255
x0,y0,x1,y1=hb; crop=out[y0:y1+1,x0:x1+1]
Image.fromarray(crop,"RGBA").save(f"{A}/asterisks.png")
vtracer.convert_image_to_svg_py(f"{A}/asterisks.png",f"{A}/asterisks.svg",colormode="color",hierarchical="stacked",mode="spline",filter_speckle=20,color_precision=4,layer_difference=80,corner_threshold=60,length_threshold=4.0,path_precision=3)
# thick curves: dilate the curve alpha by 3px before tracing (~1.5x the ~12px line)
for c in ["demand-curve","discount-curve"]:
    im=np.asarray(Image.open(f"{A}/{c}.png").convert("RGBA")).copy()
    al=im[...,3]>60; d=ndimage.binary_dilation(al,iterations=3)
    # fill new pixels with nearest colour
    idx=ndimage.distance_transform_edt(~al,return_distances=False,return_indices=True)
    filled=im[idx[0],idx[1]]; filled[...,3]=d*255
    Image.fromarray(filled,"RGBA").save(f"{A}/{c}-thick.png")
    vtracer.convert_image_to_svg_py(f"{A}/{c}-thick.png",f"{A}/{c}-thick.svg",colormode="color",hierarchical="stacked",mode="spline",filter_speckle=10,color_precision=4,layer_difference=80,corner_threshold=60,length_threshold=4.0,path_precision=3)
# smoother bee: 2x upsample, smoother spline params, 6 tone levels
bee=Image.open(f"{A}/bee.png").convert("RGBA"); bee=bee.resize((bee.width*2,bee.height*2),Image.LANCZOS); bee.save(f"{A}/bee-2x.png")
vtracer.convert_image_to_svg_py(f"{A}/bee-2x.png",f"{A}/bee-fixed.svg",colormode="color",hierarchical="stacked",mode="spline",filter_speckle=60,color_precision=6,layer_difference=36,corner_threshold=90,length_threshold=10.0,max_iterations=12,splice_threshold=60,path_precision=3)
print("done")
