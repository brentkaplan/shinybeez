import numpy as np, json, sys
from PIL import Image
from scipy import ndimage
SRC=sys.argv[1]; OUT=sys.argv[2]
DEMAND=f"{SRC}/beez-logos1.002-408.png"; DISCOUNT=f"{SRC}/beez-logos1.003-409.png"
MX0,MX1,MY0,MY1 = 10.0,230.0,11.5,265.5
PAD=12
def load(p):
    im=Image.open(p).convert("RGBA"); return np.asarray(im).astype(np.int16)
def cream(arr):
    rgb,a=arr[...,:3],arr[...,3]; op=a>30
    return np.median(rgb[op&(rgb.mean(2)>195)&((rgb.max(2)-rgb.min(2))<38)],0)
def hexbb(arr):
    ys,xs=np.where(arr[...,3]>10); return xs.min(),ys.min(),xs.max(),ys.max()
def noncream(arr,cr):
    rgb,a=arr[...,:3],arr[...,3]; return (a>30)&(np.sqrt(((rgb-cr)**2).sum(2))>34)
def beemask(arr,cr):
    nc=noncream(arr,cr); lbl,n=ndimage.label(nc,structure=np.ones((3,3)))
    s=ndimage.sum(np.ones_like(lbl),lbl,index=range(1,n+1)); return lbl==(int(np.argmax(s))+1)
def rect(bb,hb):
    fx0,fy0,fx1,fy1=hb
    mx=lambda x:MX0+(x-fx0)*(MX1-MX0)/(fx1-fx0); my=lambda y:MY0+(y-fy0)*(MY1-MY0)/(fy1-fy0)
    x0,y0,x1,y1=bb; return [round(mx(x0),2),round(my(y0),2),round(mx(x1)-mx(x0),2),round(my(y1)-my(y0),2)]
def save_mask(arr,mask,name,hb,feather=0.0):
    rgb,a=arr[...,:3],arr[...,3]; ys,xs=np.where(mask); bb=[xs.min(),ys.min(),xs.max(),ys.max()]
    out=np.zeros((*mask.shape,4),np.uint8); out[...,:3]=np.clip(rgb,0,255)
    al=(mask*np.maximum(a,0)).astype(np.uint8)
    if feather: al=ndimage.gaussian_filter(al,sigma=feather)
    out[...,3]=al
    c=[max(bb[0]-PAD,0),max(bb[1]-PAD,0),bb[2]+PAD,bb[3]+PAD]
    Image.fromarray(out[c[1]:c[3],c[0]:c[2]],"RGBA").save(OUT+"/"+name); return rect(c,hb)
def curve_comp(arr,cr,bee):
    nb=noncream(arr,cr)&~bee
    lbl,n=ndimage.label(nb,structure=np.ones((3,3)))
    objs=ndimage.find_objects(lbl)
    area=ndimage.sum(np.ones_like(lbl),lbl,index=range(1,n+1))
    bbox=[(s[1].stop-s[1].start)*(s[0].stop-s[0].start) for s in objs]
    frame=int(np.argmax(bbox))+1
    cand=[(area[i],i+1) for i in range(n) if i+1!=frame]
    cid=max(cand)[1]; return lbl==cid
res={}
ad=load(DEMAND); cd=cream(ad); bd=beemask(ad,cd); hbd=hexbb(ad)
print("master",ad.shape,"hexbb",hbd,"cream",cd)
beef=ndimage.binary_closing(ndimage.binary_fill_holes(bd),structure=np.ones((3,3)))
core=ndimage.binary_erosion(beef,iterations=4)
res["bee"]=save_mask(ad,core,"bee.png",hbd,feather=1.2)
res["demand"]=save_mask(ad,curve_comp(ad,cd,bd),"demand-curve.png",hbd)
aq=load(DISCOUNT); cq=cream(aq); bq=beemask(aq,cq); hbq=hexbb(aq)
res["discount"]=save_mask(aq,curve_comp(aq,cq,bq),"discount-curve.png",hbq)
json.dump(res,open(OUT+"/rects.json","w")); print(json.dumps(res))
