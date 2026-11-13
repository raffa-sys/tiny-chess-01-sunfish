# Sunfish: T.Ahle, GPL-3.0-only
import json,os,urllib.request as R
M=[0,100,320,330,500,900,0]
n=(-31,-33,-14,-18,14,18,31,33);d=(-17,-15,15,17);r=(-16,-1,1,16);g=d+r
Q=[6-abs(q%8-3)-abs(q//16-3)for q in range(128)]
W=[0,2,7,6,4,3,0]
Z={4:12,0:13,7:14,116:3,112:7,119:11}
A=[0]*128;J=1;E=-1;H=0;Y=[4,116];X={};KL=[0]*16;N=[0];L=15*10**4;RP=[0]*16;GH=[];DG=[];CG=-1;C=15;F=10**4;U=[]
V=os.environ['MAKEFUN_ENV_URL']
def at(q,w):
 s=1 if w else -1
 for o,v,l in(((-15*s,-17*s),1,0),(n,2,0),(g,6,0),(d,3,1),(r,4,1)):
  for t in o:
   u=q+t
   while not u&0x88:
    p=A[u]
    if p:
     if p==v*s or(l and p==5*s):return 1
     break
    if not l:break
    u+=t
 return 0
def ev():
 s=0;b=0
 for q,p in enumerate(A):
  if p:
   a=abs(p);v=M[a]+Q[q]*W[a]
   if a==1:v+=(q>>4 if p>0 else 7-(q>>4))*8
   if a==3:b+=1-2*(p<0)
   s+=v if p>0 else -v
 return s*J+10+(15*b if abs(b)==2 else 0)
def pm(f,t):
 return [f|t<<7|p<<14 for p in(5,4,3,2)]if(t>>4)==7*(J>0)else[f|t<<7]
def gn(co=0):
 mv=[]
 for f in range(128):
  p=A[f]
  if not p or p*J<=0:continue
  a=abs(p)
  if a==1:
   z=16*J;t=f+z
   if not co and A[t]==0:
    mv+=pm(f,t)
    if(f>>4)==1+5*(J<0)and A[f+2*z]==0:mv.append(f|(f+2*z)<<7)
   for w in(-1,1):
    t=f+z+w
    if t&0x88:continue
    if A[t]*J<0 or t==E:mv+=pm(f,t)
  else:
   o=n if a==2 else d if a==3 else r if a==4 else g;l=1 if a%4==2 else 9
   for z in o:
    t=f+z
    for _ in range(l):
     if t&0x88:break
     if A[t]:
      if A[t]*J<0:mv.append(f|t<<7)
      break
     if not co:mv.append(f|t<<7)
     t+=z
   if a==6 and not co:
    for k in(1,-1):
     if C>>(2*(J<0)+(k<0))&1 and A[f+k]==0==A[f+2*k]and(k>0 or A[f+3*k]==0)and not any(at(f+i*k,J<0)for i in(0,1,2)):mv.append(f|(f+2*k)<<7)
 return mv
def mk(m):
 global J,C,E,H
 f=m&127;t=m>>7&127;pr=m>>14;p=A[f];a=abs(p);cap=A[t];cs=t
 if a==1 and t==E and cap==0:cs=t-16*J;cap=A[cs];A[cs]=0
 U.append((cap,cs,C,E,H))
 A[t]=pr*J if pr else p;A[f]=0
 if a==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1);A[rt]=A[rf];A[rf]=0
 if a==6:Y[J<0]=t
 C&=Z.get(f,15)&Z.get(t,15)
 E=(f+t)//2 if a==1 and abs(t-f)==32 else -1
 H=0 if a==1 or cap else H+1;J=-J
def un(m):
 global J,C,E,H
 f=m&127;t=m>>7&127;pr=m>>14;cap,cs,C,E,H=U.pop()
 J=-J;a=abs(A[t])
 if a==6 and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1);A[rf]=A[rt];A[rt]=0
 if a==6:Y[J<0]=f
 A[f]=J if pr else A[t];A[t]=0
 if cap:A[cs]=cap
def sc(d,a,b,pl):
 N[0]+=1
 if N[0]>L:return a
 t=0
 if d<=0:
  v=ev();a=max(a,v)
  if a>=b:return b
  if pl>8:return a
  mv=gn(1)
 else:
  mv=gn()
  k=(tuple(A),J,C,E);t=X.get(k,0)
  if k in RP[:pl]or k in DG:return 0
  RP[pl]=k
 mv.sort(key=lambda m:(m==t or m==KL[pl],M[abs(A[m>>7&127])]),reverse=True)
 lc=0;bb=0
 for m in mv:
  s=J;mk(m)
  if at(Y[s<0],s<0):un(m);continue
  lc+=1;v=-sc(d-1,-b,-a,pl+1);un(m)
  if v>a:a=v;bb=m
  if a>=b:KL[pl]=m;break
 if d>0 and bb:X[k]=bb
 if lc==0 and d>0:return pl-F if at(Y[J<0],J<0) else 0
 return a
def rt():
 X.clear();N[0]=0;bb=0;RP[0]=(tuple(A),J,C,E)
 for d in range(1,5):
  v2=-F;b2=0
  for m in gn():
   s=J;mk(m)
   if not at(Y[s<0],s<0):
    v=-sc(d-1,-F,F,1)
    if v>v2:v2=v;b2=m
   un(m)
  if N[0]>L:break
  bb=b2
 return bb
def nm(f):return chr(97+(f&7))+chr(49+(f>>4))
def sf(f):
 global A,J,C,E,H;A=[0]*128;p=f.split();q=112;U.clear()
 for c in p[0]:
  if c=='/':q-=24
  elif c.isdigit():q+=int(c)
  else:
   v='PNBRQK'.index(c.upper())+1;A[q]=-v if c.islower() else v
   if v==6:Y[c=='k']=q
   q+=1
 J=1 if p[1]=='w' else -1
 C=sum(1<<'KQkq'.find(c)for c in p[2]if c in'KQkq')
 E=ord(p[3][0])-97+(ord(p[3][1])-49)*16 if p[3]!='-'else -1
 H=int(p[4])
def hk():
 k=(tuple(A),J,C,E);GH.append(k)
 if GH.count(k)>1 and k not in DG:DG.append(k)
def P(p,b):
 return json.load(R.urlopen(V+p,json.dumps(b).encode()))
if __name__=='__main__':
 while 1:
  G=P('/game',{})
  if G['status']=='done':break
  while G['status']=='your_move':
   if G['game']!=CG:GH.clear();DG.clear();CG=G['game']
   sf(G['fen']);hk()
   m=rt();mk(m);hk();un(m)
   G=P('/move',{'game':G['game'],'move':nm(m&127)+nm(m>>7&127)+('nbrq'[(m>>14)-2]if m>>14 else'')})
