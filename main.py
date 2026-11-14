# Sunfish Thomas Ahle GPL-3.0-only
import json,os
from urllib.request import Request,urlopen
V=[0,100,320,330,500,900,0]
W=[0,0,2,7,6,4,3]
Q=[6-abs(i%8-3)-abs(i//16-3) for i in range(128)]
B=[0]*128;S=1;C=0;E=-1;N=[0];L=15e4;F=4;T={};K={};R=[];Z=[];A=[0,0]
X=(15,17,-15,-17,1,-1,16,-16);O=(31,33,14,18,-31,-33,-14,-18)
U=os.getenv('MAKEFUN_ENV_URL')
def nm(i):return chr(i%8+97)+str(i//16+1)
def ky():return (tuple(B),S,C,E)
def at(t,a):
 for o in X:
  s=t+o
  while not s&136:
   if B[s]:
    if B[s]==a*5 or B[s]==a*(3 if abs(o) in(15,17) else 4):return 1
    break
   s+=o
 for o in O+X:
  s=t+o
  if not s&136 and (B[s]==a*2 and o in O or B[s]==a*6 and o in X or B[s]==a and o in(-15*a,-17*a)):return 1
 return 0
def ev():
 d=B.count(3)-B.count(-3)
 s=sum((p//(a:=abs(p)))*(V[a]+Q[i if p>0 else i^119]*W[a]+(8*(i//16 if p>0 else 7-i//16) if a==1 else 0)) for i,p in enumerate(B) if p)
 return (s+15*d*(abs(d)==2))*S+10
def gn(o=0):
 m=[];e=m.append
 for i,p in enumerate(B):
  if p*S<1:continue
  a=abs(p)
  if a==1:
   d=16*S;f=i+d
   if not B[f]:
    if f//16==(7,0)[S<0]:
     for q in (5,4,3,2):e(i|f<<7|q<<14)
    elif not o:
     e(i|f<<7)
     if i//16==(1,6)[S<0] and not B[f+d]:e(i|(f+d)<<7)
   for x in (d-1,d+1):
    t=i+x
    if t&136:continue
    if t==E or B[t]*S<0 and abs(B[t])!=6:
     if t//16==(7,0)[S<0]:
      for q in (5,4,3,2):e(i|t<<7|q<<14)
     else:e(i|t<<7)
  elif a==2 or a==6:
   for x in (O if a==2 else X):
    t=i+x
    if t&136:continue
    if B[t]*S<=0 and abs(B[t])!=6 and (B[t] or not o):e(i|t<<7)
   if a==6 and not o:
    for z,r in ((1,3),(-1,-4)):
     b=(1,2)[z<0]*(1,4)[S<0]
     if C&b and not B[i+z] and not B[i+2*z] and (z>0 or not B[i-3]) and B[i+r]==S*4 and all(not at(i+j*z,-S) for j in(0,1,2)):e(i|(i+2*z)<<7)
  else:
   for x in (X[:4] if a==3 else X[4:] if a==4 else X):
    t=i+x
    while not t&136:
     if B[t]*S>0 or B[t] in(6,-6):break
     if B[t] or not o:e(i|t<<7)
     if B[t]:break
     t+=x
 return [x for x in m if (mk(x),not at(B.index(-S*6),S),uk(x))[1]]
def mk(m):
 global S,C,E
 s=m&127;t=(m>>7)&127;q=m>>14;p=B[s];c=B[t]
 if abs(p)==1 and t==E and not c:c=B[t-16*S];B[t-16*S]=0
 Z.append((c,C,E))
 if abs(p)==6 and abs(t-s)==2:B[(s+t)//2]=B[s+3] if t>s else B[s-4];B[s+3 if t>s else s-4]=0
 B[t]=q*S if q else p;B[s]=0
 if abs(p)==6:C&=12 if S>0 else 3
 for u in(s,t):C&=~{7:1,0:2,119:4,112:8}.get(u,0)
 E=s+16*S if abs(p)==1 and abs(t-s)==32 else -1
 S=-S;R.append(ky())
def uk(m):
 global S,C,E
 s=m&127;t=(m>>7)&127;q=m>>14;c,C,E=Z.pop();R.pop();S=-S
 if B[t]==S*6 and abs(t-s)==2:B[s+3 if t>s else s-4]=B[(s+t)//2];B[(s+t)//2]=0
 B[s]=S if q else B[t]
 if t==E and c:B[t-16*S]=c;B[t]=0
 else:B[t]=c
def sr(d,a,b,p):
 N[0]+=1;A[0]|=N[0]>L
 if A[0]:return 0
 k=ky()
 if p and k in R[:-1]:return 0
 if d<=0:
  v=ev()
  if v>=b:return b
  a=max(a,v)
  if d<-11:return a
  ms=gn(1)
 else:
  ms=gn()
  if not ms:return p-9999 if S*6 in B and at(B.index(S*6),-S) else 0
 ms.sort(key=lambda m:(t:=(m>>7)&127,999 if m==T.get(k) else 950 if m==K.get(p) else (V[abs(B[t])] or 100) if B[t] or t==E else 0)[1],reverse=True)
 w=0
 for m in ms:
  mk(m);v=-sr(d-1,-b,-a,p+1);uk(m)
  if A[0]:return 0
  if v>a:
   a=v;w=m
   if not p:A[1]=m
  if a>=b:break
 if d>0:
  if a>=b:K[p]=w
  T[k]=w
 return a
def rt():
 N[0]=0;A[:]=[0,0];T.clear();K.clear();r=0
 for d in(1,2,3,4,5):
  sr(d,-99999,99999,0)
  if A[0]:break
  r=A[1]
 return r or gn()[0]
def sf(f):
 global S,C,E,B
 w=f.split();B=[0]*128;r=7;n=0
 for c in w[0]:
  if c=='/':r-=1;n=0
  elif c.isdigit():n+=int(c)
  else:B[16*r+n]=("PNBRQK".find(c.upper())+1)*(1,-1)[c>'Z'];n+=1
 S=1-(w[1]=='b')*2;C=sum(1<<'KQkq'.find(c) for c in w[2] if c!='-');E=ord(w[3][0])+16*int(w[3][1])-113 if w[3]!='-' else -1;R[:]=[ky()]
def po(p,d):
 q=Request(U+p,json.dumps(d).encode(),{'Content-Type':'application/json'});return json.load(urlopen(q))
if __name__=='__main__':
 while 1:
  try:
   s=po('/game',{});x=s['status']
   if x[0]=='d':break
   if x[0]=='y':sf(s['fen']);m=rt();po('/move',{'game':s['game'],'move':nm(m&127)+nm(m>>7&127)+('nbrq'[(m>>14)-2] if m>>14 else '')})
  except:pass
