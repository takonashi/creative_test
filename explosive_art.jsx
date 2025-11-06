import React, { useEffect, useRef, useState } from "react";
import { Button } from "@/components/ui/button";
import { Pause, Play, Shuffle, Download, Bomb, Volume2, VolumeX } from "lucide-react";

const frag = `
precision highp float;
uniform vec2 u_res;
uniform float u_time;
uniform vec2 u_mouse;
uniform float u_seed;
uniform int u_blastCount;
uniform vec4 u_blasts[8];
uniform int u_cutCount;
uniform vec4 u_cuts[16];
uniform int u_boltCount;
uniform vec4 u_bolts[16];
uniform float u_q;

float hash(vec2 p){return fract(sin(dot(p,vec2(127.1,311.7)))*43758.5453123);} 
float noise(vec2 x){vec2 i=floor(x);vec2 f=fract(x);float a=hash(i+vec2(0.0,0.0));float b=hash(i+vec2(1.0,0.0));float c=hash(i+vec2(0.0,1.0));float d=hash(i+vec2(1.0,1.0));vec2 u=f*f*(3.0-2.0*f);return mix(a,b,u.x)+(c-a)*u.y*(1.0-u.x)+(d-b)*u.x*u.y;}
float fbm_q(vec2 x, float oct){float v=0.0;float a=0.5;for(int i=0;i<6;i++){if(float(i)>=oct) break;v+=a*noise(x);x=x*2.0+vec2(13.1,7.7);a*=0.5;}return v;}

void main(){
  vec2 uv=gl_FragCoord.xy/u_res.xy;
  float aspect=u_res.x/u_res.y;
  vec2 p=(uv-0.5)*vec2(aspect,1.0);

  float imp=0.0;
  for(int i=0;i<8;i++){
    if(i>=u_blastCount) break;
    vec2 bc=u_blasts[i].xy; float ts=u_blasts[i].z; float age=max(u_time-ts,0.0);
    float n=exp(-6.0*length(p-bc))*exp(-0.35*age);
    imp+=n;
  }

  vec2 p2=p;
  vec2 sh=0.02*imp*vec2(sin(100.0*u_time+p.y*30.0),cos(130.0*u_time-p.x*25.0));
  p2+=sh;
  float slicer=step(0.97, fract(sin((uv.y*10.0+u_time*4.0)*12.345+u_seed*50.0)*43758.5453));
  p2.x += (hash(vec2(uv.y*10.0,u_time))-0.5)*0.25*imp*slicer;

  float cutGlow=0.0;
  for(int i=0;i<16;i++){
    if(i>=u_cutCount) break;
    vec2 cp=u_cuts[i].xy; float ts=u_cuts[i].z; float ang=u_cuts[i].w;
    float age=max(u_time-ts,0.0);
    vec2 dir=vec2(cos(ang), sin(ang));
    vec2 nor=vec2(-sin(ang), cos(ang));
    vec2 dpos=p2-cp;
    float d=abs(dot(nor,dpos));
    float l=dot(dir,dpos);
    float L=0.18+age*0.7;
    float gate=1.0-smoothstep(L-0.02,L,abs(l));
    float w=0.018+0.06*min(age,1.0);
    float s=d/(w+1e-3);
    float edge=exp(-s*s*18.0)*gate;
    p2 += nor*edge*sign(l)*(0.08*exp(-0.25*age));
    float ch=hash(vec2(float(i)+l*30.0,u_time*60.0));
    p2.x += (ch-0.5)*0.02*edge;
    cutGlow += edge;
  }

  float boltGlow=0.0;
  vec2 boltPush=vec2(0.0);
  for(int b=0;b<16;b++){
    if(b>=u_boltCount) break;
    vec2 s0=u_bolts[b].xy; float ts=u_bolts[b].z; float ang=u_bolts[b].w;
    float age=max(u_time-ts,0.0);
    float len=min(1.2, 0.22+age*0.9);
    vec2 dir=vec2(cos(ang), sin(ang));
    vec2 nor=vec2(-dir.y, dir.x);
    float md=1e9;
    for(int j=0;j<22;j++){
      if(u_q<0.75 && mod(float(j),2.0)>0.5) continue;
      if(u_q<0.60 && mod(float(j),3.0)>0.5) continue;
      if(u_q<0.50 && mod(float(j),4.0)>0.5) continue;
      if(u_q<0.35 && mod(float(j),5.0)>0.5) continue;
      float seg=float(j)/21.0;
      vec2 q=s0 + dir*(len*seg);
      float z;
      if(u_q>0.6){
        z = sin((ang*17.0 + seg*130.0) + fbm_q(vec2(seg*35.0, ang*9.0 + u_time*3.0), 2.0+3.0*u_q)*2.0);
      } else {
        z = sin(ang*17.0 + seg*130.0 + u_time*3.0 + hash(vec2(seg,ang))*2.0);
      }
      float amp = (0.05 + 0.07*seg);
      q += nor*z*amp;
      if(mod(float(j),5.0)==0.0){
        float a2 = ang + (hash(vec2(float(j),ang*13.0))-0.5)*1.8;
        vec2 d2=vec2(cos(a2),sin(a2));
        vec2 qb=q + d2*(0.18*seg);
        md=min(md, length(p2-qb));
      }
      md=min(md, length(p2-q));
      if(md<0.004*(1.0+0.8*(1.0-u_q))) break;
    }
    float core=exp(-90.0*md);
    float glow=exp(-28.0*md);
    boltGlow += glow*(1.0 - smoothstep(0.9,1.8,age));
    boltPush += nor * (0.07*exp(-0.45*age)) * (glow*sign(dot(p2-s0,dir)));
    imp += core*0.6 + glow*0.3;
  }
  p2 += boltPush;

  float tt=u_time*0.12+u_seed*10.0;
  float oct=2.0+4.0*u_q;
  vec2 qq=vec2(fbm_q(p2*2.0+vec2(tt,-tt),oct), fbm_q(p2*2.2-vec2(tt,tt),oct));
  float f=fbm_q(p2*3.0+qq*1.6+vec2(0.0,tt),oct);

  vec2 m=(u_mouse-0.5)*vec2(aspect,1.0);
  float mg=0.3*exp(-6.0*length(p2-m));
  f+=mg + cutGlow*0.9 + boltGlow*1.2;

  for(int i=0;i<8;i++){
    if(i>=u_blastCount) break;
    vec2 bc=u_blasts[i].xy; float ts=u_blasts[i].z; float age=max(u_time-ts,0.0);
    float ring=exp(-14.0*abs(length(p2-bc)-(age*0.38+0.03*sin(30.0*age))));
    float shock=ring*exp(-0.22*age);
    f+=shock*0.72;
  }

  float base=f+tt*0.25;
  vec3 col=0.5+0.5*cos(6.28318*(vec3(0.0,0.33,0.67)+base+vec3(0.02*imp+0.05*cutGlow+0.08*boltGlow,0.0,-0.02*imp)));

  float dread=smoothstep(0.05,0.6,imp+0.6*cutGlow+0.8*boltGlow);
  float flash=smoothstep(0.2,1.0,imp+cutGlow+boltGlow*1.3);
  col=mix(col, vec3(0.9,0.05,0.02), 0.33*dread);
  col=mix(col, vec3(1.0,0.95,1.0), clamp(boltGlow*1.2,0.0,1.0));
  col=pow(col, vec3(1.08+0.28*dread));
  col=mix(col, vec3(1.0), 0.58*flash);

  float vign=smoothstep(1.15,0.28,length(p2));
  col*=vign+0.08;

  float grain=hash(uv*u_res.xy+u_time)*0.03*(0.2+0.8*u_q);
  col+=grain;
  col=clamp(col,0.0,1.0);

  gl_FragColor=vec4(col,1.0);
}
`;

const vert = `
attribute vec2 a_pos;
void main(){gl_Position=vec4(a_pos,0.0,1.0);} 
`;

function createShader(gl, type, source){
  const sh=gl.createShader(type);
  gl.shaderSource(sh,source);
  gl.compileShader(sh);
  if(!gl.getShaderParameter(sh,gl.COMPILE_STATUS)){
    const log=gl.getShaderInfoLog(sh);
    gl.deleteShader(sh);
    throw new Error("Shader compile error: "+log);
  }
  return sh;
}

function createProgram(gl, vsSrc, fsSrc){
  const vs=createShader(gl,gl.VERTEX_SHADER,vsSrc);
  const fs=createShader(gl,gl.FRAGMENT_SHADER,fsSrc);
  const prog=gl.createProgram();
  gl.attachShader(prog,vs);
  gl.attachShader(prog,fs);
  gl.linkProgram(prog);
  if(!gl.getProgramParameter(prog,gl.LINK_STATUS)){
    const log=gl.getProgramInfoLog(prog);
    gl.deleteProgram(prog);
    throw new Error("Program link error: "+log);
  }
  return {prog,vs,fs};
}

export default function ExplosiveArt(){
  const canvasRef=useRef(null);
  const glRef=useRef(null);
  const progRef=useRef(null);
  const vsRef=useRef(null);
  const fsRef=useRef(null);
  const bufRef=useRef(null);
  const rafRef=useRef(0);
  const startRef=useRef(0);
  const pauseAtRef=useRef(0);
  const nextAutoAtRef=useRef(0);
  const [running,setRunning]=useState(true);
  const [seed,setSeed]=useState(()=>Math.random());
  const [muted,setMuted]=useState(false);
  const runningRef=useRef(true);
  const seedRef=useRef(0);
  const blastsRef=useRef([]);
  const blastBufRef=useRef(new Float32Array(8*4));
  const cutsRef=useRef([]);
  const cutBufRef=useRef(new Float32Array(16*4));
  const boltsRef=useRef([]);
  const boltBufRef=useRef(new Float32Array(16*4));
  const drawRef=useRef(null);
  const audioRef=useRef(null);
  const isDownRef=useRef(false);
  const lastDragRef=useRef({x:0,y:0,t:0});
  const lastClickRef=useRef(0);
  const lastZapRef=useRef(0);
  const scaleRef=useRef(1);
  const perfRef=useRef({avg:16.7,last:performance.now(),frames:0});

  const initAudio=()=>{
    if(audioRef.current) return audioRef.current;
    const Ctx=window.AudioContext||window.webkitAudioContext; if(!Ctx) return null;
    const ctx=new Ctx();

    const master=ctx.createGain(); master.gain.value=0.9;
    const comp=ctx.createDynamicsCompressor();

    const dry=ctx.createGain(); dry.gain.value=0.7;
    const wet=ctx.createGain(); wet.gain.value=0.55;

    const makeIR=(seconds=2.2, decay=2.5)=>{
      const len=Math.floor(ctx.sampleRate*seconds);
      const ir=ctx.createBuffer(2,len,ctx.sampleRate);
      for(let ch=0; ch<2; ch++){
        const data=ir.getChannelData(ch);
        for(let i=0;i<len;i++){
          const u=i/len;
          const noise=(Math.random()*2.0-1.0);
          data[i]=noise*Math.pow(1.0-u, decay);
        }
      }
      return ir;
    };
    const convolver=ctx.createConvolver();
    convolver.buffer=makeIR(2.8,3.2);

    const delay=ctx.createDelay(3.0); delay.delayTime.value=0.28;
    const fb=ctx.createGain(); fb.gain.value=0.38;
    const tone=ctx.createBiquadFilter(); tone.type='lowpass'; tone.frequency.value=1800; tone.Q.value=0.7;

    master.connect(dry);
    master.connect(convolver);
    master.connect(delay);

    convolver.connect(wet);
    delay.connect(tone);
    tone.connect(fb);
    fb.connect(delay);
    tone.connect(wet);

    const limiter=ctx.createDynamicsCompressor();
    limiter.threshold.value=-6;
    limiter.knee.value=0;
    limiter.ratio.value=20;
    limiter.attack.value=0.003;
    limiter.release.value=0.2;

    dry.connect(comp);
    wet.connect(comp);
    comp.connect(limiter);
    limiter.connect(ctx.destination);

    const noiseBuffer=ctx.createBuffer(1, ctx.sampleRate*2, ctx.sampleRate);
    const ch=noiseBuffer.getChannelData(0); for(let i=0;i<ch.length;i++){ ch[i]=Math.random()*2-1; }

    const curve=(()=>{const n=4096;const arr=new Float32Array(n);for(let i=0;i<n;i++){const x=i*2/n-1;arr[i]=Math.tanh(3.2*x);}return arr;})();

    audioRef.current={ctx, master, comp, limiter, dry, wet, convolver, delay, fb, tone, noiseBuffer, curve, drag:null};
    return audioRef.current;
  };

  const ensureAudio=()=>{ const a=initAudio(); if(a&&a.ctx.state!=='running'){ a.ctx.resume(); } return a; };

  const startTearSound=(cx)=>{
    const a=ensureAudio(); if(!a) return;
    if(a.drag) return;
    const {ctx, noiseBuffer, curve}=a;
    const pan=ctx.createStereoPanner?ctx.createStereoPanner():null;
    const hp=ctx.createBiquadFilter(); hp.type='highpass'; hp.frequency.value=1200;
    const bp=ctx.createBiquadFilter(); bp.type='bandpass'; bp.Q.value=18; bp.frequency.value=3200;
    const notch=ctx.createBiquadFilter(); notch.type='notch'; notch.Q.value=6; notch.frequency.value=2500;
    const sh=ctx.createWaveShaper(); sh.curve=curve; sh.oversample='4x';
    const comb=ctx.createDelay(0.06); comb.delayTime.value=0.018;
    const fb=ctx.createGain(); fb.gain.value=0.6; comb.connect(fb); fb.connect(comb);
    const g=ctx.createGain(); g.gain.value=0.0001;

    const n=ctx.createBufferSource(); n.buffer=noiseBuffer; n.loop=true;
    const saw=ctx.createOscillator(); saw.type='sawtooth'; saw.frequency.value=2400;
    n.connect(hp); hp.connect(bp); bp.connect(notch); notch.connect(sh); sh.connect(comb); comb.connect(g);
    saw.connect(bp);
    if(pan){ g.connect(pan); pan.pan.value=cx; pan.connect(a.master); } else { g.connect(a.master); }
    g.connect(a.wet);

    const t=ctx.currentTime; g.gain.exponentialRampToValueAtTime(0.8, t+0.04);
    saw.start(); n.start();
    a.drag={n,saw,hp,bp,notch,sh,comb,fb,g,pan};
  };
  const updateTearSound=(cx,speed)=>{
    const a=audioRef.current; if(!a) return;
    if(a.ctx.state!=='running'){ a.ctx.resume(); }
    if(!a.drag && isDownRef.current){ startTearSound(cx); }
    if(!a.drag) return;
    const {ctx}=a; const now=ctx.currentTime;
    const k=Math.min(1.0,speed*2.0);
    const f=800.0+4200.0*k;
    const dl=0.01+0.03*k;
    a.drag.bp.frequency.setTargetAtTime(f, now, 0.01);
    a.drag.notch.frequency.setTargetAtTime(1800.0+2200.0*k, now, 0.02);
    a.drag.comb.delayTime.setTargetAtTime(dl, now, 0.015);
    if(a.drag.pan){ a.drag.pan.pan.setTargetAtTime(cx, now, 0.02); }
    a.drag.g.gain.setTargetAtTime(0.6+0.6*k, now, 0.01);
    if(a.drag.saw){ a.drag.saw.frequency.setTargetAtTime(1800.0+3600.0*k, now, 0.015); }
  };
  const stopTearSound=()=>{
    const a=audioRef.current; if(!a||!a.drag) return;
    const {ctx}=a; const now=ctx.currentTime;
    a.drag.g.gain.setTargetAtTime(0.0001, now, 0.03);
    try{ a.drag.n.stop(now+0.12); a.drag.n.onended=()=>{ try{a.drag.n.disconnect();}catch{} }; }catch(e){}
    try{ a.drag.saw.stop(now+0.12); a.drag.saw.onended=()=>{ try{a.drag.saw.disconnect();}catch{} }; }catch(e){}
    a.drag=null;
  };

  const playBlastSound=(cx, cy, amp=1)=>{
    const a=ensureAudio(); if(!a) return;
    const {ctx, noiseBuffer}=a;
    const now=ctx.currentTime;
    const aspect=(canvasRef.current?.width||1)/(canvasRef.current?.height||1);
    const panVal=Math.max(-1, Math.min(1, cx/(0.5*aspect)));
    const base=Math.max(0.2, Math.min(1, amp));

    const boomOsc=ctx.createOscillator();
    boomOsc.type='sine';
    const boomGain=ctx.createGain();
    boomGain.gain.setValueAtTime(0.0001, now);
    boomGain.gain.exponentialRampToValueAtTime(0.8*base, now+0.005);
    boomGain.gain.exponentialRampToValueAtTime(0.0001, now+1.2);
    boomOsc.frequency.setValueAtTime(80-40*(cy+0.5), now);
    boomOsc.frequency.exponentialRampToValueAtTime(25, now+0.9);
    const pan1=ctx.createStereoPanner?ctx.createStereoPanner():null;
    if(pan1){ boomGain.connect(pan1); pan1.pan.setValueAtTime(panVal, now); pan1.connect(a.master); } else { boomGain.connect(a.master); }
    boomOsc.connect(boomGain);
    boomOsc.start(now); boomOsc.stop(now+1.2);
    boomOsc.onended=()=>{ try{boomGain.disconnect();}catch{} try{pan1&&pan1.disconnect();}catch{} };

    const noise=ctx.createBufferSource(); noise.buffer=noiseBuffer;
    const bp=ctx.createBiquadFilter(); bp.type='bandpass'; bp.frequency.setValueAtTime(1500, now); bp.Q.setValueAtTime(0.7, now);
    const nGain=ctx.createGain(); nGain.gain.setValueAtTime(0.0001, now); nGain.gain.exponentialRampToValueAtTime(0.7*base, now+0.002); nGain.gain.exponentialRampToValueAtTime(0.0001, now+0.35);
    const pan2=ctx.createStereoPanner?ctx.createStereoPanner():null; if(pan2){ nGain.connect(pan2); pan2.pan.setValueAtTime(panVal, now); pan2.connect(a.master);} else { nGain.connect(a.master); }
    noise.connect(bp); bp.connect(nGain); noise.start(now); noise.stop(now+0.4);
    noise.onended=()=>{ try{bp.disconnect();}catch{} try{nGain.disconnect();}catch{} try{pan2&&pan2.disconnect();}catch{} };

    const crack=ctx.createBufferSource(); crack.buffer=noiseBuffer;
    const hp=ctx.createBiquadFilter(); hp.type='highpass'; hp.frequency.setValueAtTime(4000, now);
    const cGain=ctx.createGain(); cGain.gain.setValueAtTime(0.7*base, now); cGain.gain.exponentialRampToValueAtTime(0.0001, now+0.06);
    const pan3=ctx.createStereoPanner?ctx.createStereoPanner():null; if(pan3){ cGain.connect(pan3); pan3.pan.setValueAtTime(panVal, now); pan3.connect(a.master);} else { cGain.connect(a.master); }
    crack.connect(hp); hp.connect(cGain); crack.start(now); crack.stop(now+0.08);
    crack.onended=()=>{ try{hp.disconnect();}catch{} try{cGain.disconnect();}catch{} try{pan3&&pan3.disconnect();}catch{} };

    if(a.wet){ a.wet.gain.setTargetAtTime(0.8, now, 0.01); a.wet.gain.setTargetAtTime(0.55, now+1.2, 0.5); }
  };

  const playClickShockwave=(cx, cy, amp=2)=>{
    const a=ensureAudio(); if(!a) return;
    const {ctx, noiseBuffer, curve}=a;
    const now=ctx.currentTime;
    const aspect=(canvasRef.current?.width||1)/(canvasRef.current?.height||1);
    const panVal=Math.max(-1, Math.min(1, cx/(0.5*aspect)));
    const base=Math.max(0.6, Math.min(2.5, amp));

    const nsrc=ctx.createBufferSource(); nsrc.buffer=noiseBuffer;
    const hp1=ctx.createBiquadFilter(); hp1.type='highpass'; hp1.frequency.setValueAtTime(500, now);
    const bp=ctx.createBiquadFilter(); bp.type='bandpass'; bp.Q.setValueAtTime(10, now); bp.frequency.setValueAtTime(8000, now); bp.frequency.exponentialRampToValueAtTime(220, now+0.25);
    const sh=ctx.createWaveShaper(); sh.curve=curve; sh.oversample='4x';
    const nGain=ctx.createGain(); nGain.gain.setValueAtTime(0.0001, now); nGain.gain.exponentialRampToValueAtTime(2.0*base, now+0.004); nGain.gain.exponentialRampToValueAtTime(0.0001, now+0.42);
    const pan=ctx.createStereoPanner?ctx.createStereoPanner():null;
    if(pan){ nsrc.connect(hp1); hp1.connect(bp); bp.connect(sh); sh.connect(nGain); nGain.connect(pan); pan.pan.setValueAtTime(panVal, now); pan.connect(a.master);} else { nsrc.connect(hp1); hp1.connect(bp); bp.connect(sh); sh.connect(nGain); nGain.connect(a.master);} 
    nsrc.start(now); nsrc.stop(now+0.5);
    nsrc.onended=()=>{ try{hp1.disconnect();}catch{} try{bp.disconnect();}catch{} try{sh.disconnect();}catch{} try{nGain.disconnect();}catch{} try{pan&&pan.disconnect();}catch{} };

    const sub=ctx.createOscillator(); sub.type='sine';
    sub.frequency.setValueAtTime(90, now);
    sub.frequency.exponentialRampToValueAtTime(18, now+0.35);
    const sg=ctx.createGain(); sg.gain.setValueAtTime(0.0001, now); sg.gain.exponentialRampToValueAtTime(1.2*base, now+0.01); sg.gain.exponentialRampToValueAtTime(0.0001, now+0.5);
    const lpf=ctx.createBiquadFilter(); lpf.type='lowpass'; lpf.frequency.setValueAtTime(180, now);
    const panS=ctx.createStereoPanner?ctx.createStereoPanner():null;
    if(panS){ sub.connect(lpf); lpf.connect(sg); sg.connect(panS); panS.pan.setValueAtTime(panVal*0.2, now); panS.connect(a.master);} else { sub.connect(lpf); lpf.connect(sg); sg.connect(a.master);} 
    sub.start(now); sub.stop(now+0.5);
    sub.onended=()=>{ try{lpf.disconnect();}catch{} try{sg.disconnect();}catch{} try{panS&&panS.disconnect();}catch{} };

    const crack=ctx.createBufferSource(); crack.buffer=noiseBuffer;
    const hp2=ctx.createBiquadFilter(); hp2.type='highpass'; hp2.frequency.setValueAtTime(6500, now);
    const cg=ctx.createGain(); cg.gain.setValueAtTime(1.5*base, now); cg.gain.exponentialRampToValueAtTime(0.0001, now+0.05);
    const panC=ctx.createStereoPanner?ctx.createStereoPanner():null;
    if(panC){ crack.connect(hp2); hp2.connect(cg); cg.connect(panC); panC.pan.setValueAtTime(panVal, now); panC.connect(a.master);} else { crack.connect(hp2); hp2.connect(cg); cg.connect(a.master);} 
    crack.start(now); crack.stop(now+0.08);
    crack.onended=()=>{ try{hp2.disconnect();}catch{} try{cg.disconnect();}catch{} try{panC&&panC.disconnect();}catch{} };

    if(a.wet){ a.wet.gain.setTargetAtTime(0.95, now, 0.005); a.wet.gain.setTargetAtTime(0.55, now+1.0, 0.5); }
  };

  const playHeartbeatSound=(cx, cy, amp=1)=>{
    const a=ensureAudio(); if(!a) return;
    const {ctx}=a;
    const now=ctx.currentTime;
    const aspect=(canvasRef.current?.width||1)/(canvasRef.current?.height||1);
    const panBase=Math.max(-1, Math.min(1, cx/(0.5*aspect)));
    const lvl=Math.max(0.2, Math.min(1, amp));

    const beat=(off, g0, panSpread)=>{
      const t=now+off;
      const osc=ctx.createOscillator(); osc.type='sine';
      const g=ctx.createGain();
      g.gain.setValueAtTime(0.0001, t);
      g.gain.exponentialRampToValueAtTime(g0, t+0.01);
      g.gain.exponentialRampToValueAtTime(0.0001, t+0.22);
      osc.frequency.setValueAtTime(150, t);
      osc.frequency.exponentialRampToValueAtTime(55, t+0.18);
      const pan=ctx.createStereoPanner?ctx.createStereoPanner():null;
      if(pan){ pan.pan.setValueAtTime(panBase*0.2, t); pan.pan.linearRampToValueAtTime(panBase*panSpread, t+0.25); g.connect(pan); pan.connect(a.master);} else { g.connect(a.master); }
      osc.connect(g); osc.start(t); osc.stop(t+0.25);
      osc.onended=()=>{ try{g.disconnect();}catch{} try{pan&&pan.disconnect();}catch{} };
    };

    beat(0.00, 0.9*lvl, 0.9);
    beat(0.22, 0.7*lvl, 1.0);

    const r=ctx.createOscillator(); r.type='sine';
    const rg=ctx.createGain(); const t0=now;
    rg.gain.setValueAtTime(0.0001, t0);
    rg.gain.exponentialRampToValueAtTime(0.5*lvl, t0+0.02);
    rg.gain.exponentialRampToValueAtTime(0.0001, t0+1.2);
    r.frequency.setValueAtTime(40, t0);
    const panr=ctx.createStereoPanner?ctx.createStereoPanner():null;
    if(panr){ panr.pan.setValueAtTime(panBase*0.1, t0); rg.connect(panr); panr.connect(a.master);} else { rg.connect(a.master); }
    r.connect(rg); r.start(t0); r.stop(t0+1.2);
    r.onended=()=>{ try{rg.disconnect();}catch{} try{panr&&panr.disconnect();}catch{} };

    if(a.wet){ a.wet.gain.setTargetAtTime(0.9, now, 0.005); a.wet.gain.setTargetAtTime(0.55, now+1.3, 0.6); }
  };

  const playZap=(cx,intensity=1)=>{
    const a=ensureAudio(); if(!a) return;
    const {ctx, noiseBuffer, curve}=a; const now=ctx.currentTime;
    const n=ctx.createBufferSource(); n.buffer=noiseBuffer; n.loop=false;
    const hp=ctx.createBiquadFilter(); hp.type='highpass'; hp.frequency.value=2500;
    const bp=ctx.createBiquadFilter(); bp.type='bandpass'; bp.Q.value=25; bp.frequency.setValueAtTime(6000, now); bp.frequency.exponentialRampToValueAtTime(1200, now+0.12);
    const sh=ctx.createWaveShaper(); sh.curve=curve; sh.oversample='4x';
    const g=ctx.createGain(); g.gain.setValueAtTime(0.0001, now); g.gain.exponentialRampToValueAtTime(1.2*intensity, now+0.004); g.gain.exponentialRampToValueAtTime(0.0001, now+0.16);
    const pan=ctx.createStereoPanner?ctx.createStereoPanner():null; if(pan){ g.connect(pan); pan.pan.value=cx; pan.connect(a.master);} else { g.connect(a.master); }
    n.connect(hp); hp.connect(bp); bp.connect(sh); sh.connect(g);
    n.start(now); n.stop(now+0.18);
    n.onended=()=>{ try{hp.disconnect();}catch{} try{bp.disconnect();}catch{} try{sh.disconnect();}catch{} try{g.disconnect();}catch{} try{pan&&pan.disconnect();}catch{} };
  };

  const randomize=()=>{const s=Math.random();setSeed(s);};
  const toggle=()=>{
    setRunning(r=>{
      const n=!r; runningRef.current=n;
      if(!n){
        pauseAtRef.current=performance.now();
        stopTearSound();
        if(rafRef.current){cancelAnimationFrame(rafRef.current);rafRef.current=0;}
      } else {
        ensureAudio();
        if(pauseAtRef.current){const d=performance.now()-pauseAtRef.current;startRef.current+=d;pauseAtRef.current=0;}
        if(drawRef.current&&!rafRef.current){rafRef.current=requestAnimationFrame(drawRef.current);} 
      }
      return n;
    });
  };
  const toggleMute=()=>{
    setMuted(m=>{
      const nm=!m; const a=ensureAudio(); if(a){ a.master.gain.setTargetAtTime(nm?0.0:0.9, a.ctx.currentTime, 0.02);} return nm;
    });
  };
  const savePNG=()=>{
    const c=canvasRef.current; if(!c) return;
    const out=document.createElement("canvas"); out.width=c.width; out.height=c.height;
    const ctx=out.getContext("2d"); ctx.drawImage(c,0,0);
    const a=document.createElement("a"); a.href=out.toDataURL("image/png"); a.download="explosive-art.png"; a.click();
  };

  useEffect(()=>{seedRef.current=seed;},[seed]);

  useEffect(()=>{
    const c=canvasRef.current; if(!c) return;
    const gl=c.getContext("webgl",{antialias:true}); if(!gl) throw new Error("WebGL not supported"); glRef.current=gl;
    const {prog,vs,fs}=createProgram(gl,vert,frag); progRef.current=prog; vsRef.current=vs; fsRef.current=fs; gl.useProgram(prog);
    const buf=gl.createBuffer(); bufRef.current=buf; gl.bindBuffer(gl.ARRAY_BUFFER,buf);
    gl.bufferData(gl.ARRAY_BUFFER,new Float32Array([-1,-1,1,-1,-1,1,1,-1,1,1,-1,1]),gl.STATIC_DRAW);
    const aPos=gl.getAttribLocation(prog,"a_pos"); gl.enableVertexAttribArray(aPos); gl.vertexAttribPointer(aPos,2,gl.FLOAT,false,0,0);

    startRef.current=performance.now(); nextAutoAtRef.current=1.0+Math.random()*1.5;

    const resize=()=>{ const dpr=Math.min(window.devicePixelRatio||1,2);
      const s=Math.max(0.6, Math.min(1.25, scaleRef.current));
      const w=Math.floor(c.clientWidth*dpr*s); const h=Math.floor(c.clientHeight*dpr*s);
      if(c.width!==w||c.height!==h){ c.width=w; c.height=h; gl.viewport(0,0,w,h);} };
    resize(); const ro=new ResizeObserver(resize); ro.observe(c);

    const mouse={x:0.5,y:0.5};
    const toCanvas=(clientX,clientY)=>{ const r=c.getBoundingClientRect();
      const x=(clientX-r.left)/r.width; const y=1-(clientY-r.top)/r.height; const aspect=c.width/c.height; return {cx:(x-0.5)*aspect, cy:(y-0.5)}; };

    const onMove=(e)=>{
      const r=c.getBoundingClientRect(); mouse.x=Math.max(0,Math.min(1,(e.clientX-r.left)/r.width)); mouse.y=Math.max(0,Math.min(1,1-(e.clientY-r.top)/r.height));
      if(isDownRef.current){
        const a=ensureAudio(); if(a&&a.ctx.state!=='running'){ a.ctx.resume(); }
        if(!audioRef.current?.drag){ startTearSound(mouse.x*2.0-1.0); }
        const {cx,cy}=toCanvas(e.clientX,e.clientY);
        const now=(performance.now()-startRef.current)/1000; const last=lastDragRef.current;
        const dx=cx-last.x, dy=cy-last.y; const dt=Math.max(0.001, now-last.t); const dist=Math.hypot(dx,dy); const speed=dist/dt;
        if(dist>0.003){
          const ang=Math.atan2(dy,dx);
          const q=((scaleRef.current-0.6)/(1.25-0.6));
          cutsRef.current=[...cutsRef.current,{x:cx,y:cy,t:now,a:ang}].slice(-16);
          const dense=q>0.65; const n=(dense?2:1)+Math.floor(Math.random()*(dense?3:2));
          const arr=[]; for(let i=0;i<n;i++){ const a=ang + (Math.random()-0.5)*(0.8+1.6*Math.min(1.5,speed)); arr.push({x:cx,y:cy,t:now,a}); }
          boltsRef.current=[...boltsRef.current,...arr].slice(-16);
          const nowMs=performance.now(); if(nowMs-lastZapRef.current>40){ playZap(mouse.x*2.0-1.0, 0.6+Math.min(1.2,speed)); lastZapRef.current=nowMs; }
          updateTearSound(mouse.x*2.0-1.0, Math.min(1.5,speed));
          lastDragRef.current={x:cx,y:cy,t:now};
        }
      }
    };
    const onDown=(e)=>{
      isDownRef.current=true; ensureAudio();
      try{ c.setPointerCapture && c.setPointerCapture(e.pointerId); }catch{}
      const {cx,cy}=toCanvas(e.clientX,e.clientY); const now=(performance.now()-startRef.current)/1000;
      lastDragRef.current={x:cx,y:cy,t:now}; startTearSound(mouse.x*2.0-1.0);
      cutsRef.current=[...cutsRef.current,{x:cx,y:cy,t:now,a:0.0}].slice(-16);
      const q=((scaleRef.current-0.6)/(1.25-0.6)); const dense=q>0.65; const n=(dense?3:2)+Math.floor(Math.random()*(dense?3:2));
      const arr=[]; for(let i=0;i<n;i++){ const a=(Math.random()*6.28318); arr.push({x:cx,y:cy,t:now,a}); }
      boltsRef.current=[...boltsRef.current,...arr].slice(-16);
      lastZapRef.current=performance.now();
      playZap(mouse.x*2.0-1.0, 1.0);
    };
    const endDrag=(e)=>{ if(isDownRef.current){ isDownRef.current=false; stopTearSound(); try{ c.releasePointerCapture && e && e.pointerId!==undefined && c.releasePointerCapture(e.pointerId); }catch{} }};

    c.addEventListener("pointermove",onMove);
    c.addEventListener("pointerdown",onDown);
    window.addEventListener("pointerup",endDrag);
    c.addEventListener("pointercancel",endDrag);

    const onClick=(e)=>{
      const nowMs=performance.now();
      if(nowMs - lastClickRef.current < 160) return;
      lastClickRef.current=nowMs;
      const {cx,cy}=toCanvas(e.clientX,e.clientY);
      const now=(performance.now()-startRef.current)/1000;
      blastsRef.current=[...blastsRef.current,{x:cx,y:cy,t:now}].slice(-6);
      playClickShockwave(cx,cy,1.8);
    };
    c.addEventListener("click",onClick);

    const uRes=gl.getUniformLocation(prog,"u_res");
    const uTime=gl.getUniformLocation(prog,"u_time");
    const uMouse=gl.getUniformLocation(prog,"u_mouse");
    const uSeed=gl.getUniformLocation(prog,"u_seed");
    const uBlastCount=gl.getUniformLocation(prog,"u_blastCount");
    const uBlasts=gl.getUniformLocation(prog,"u_blasts[0]");
    const uCutCount=gl.getUniformLocation(prog,"u_cutCount");
    const uCuts=gl.getUniformLocation(prog,"u_cuts[0]");
    const uBoltCount=gl.getUniformLocation(prog,"u_boltCount");
    const uBolts=gl.getUniformLocation(prog,"u_bolts[0]");
    const uQ=gl.getUniformLocation(prog,"u_q");

    seedRef.current = seed; runningRef.current = true;

    const draw=()=>{
      if(!runningRef.current){rafRef.current=0;return;}

      const nowMs=performance.now();
      const dt=nowMs-perfRef.current.last; perfRef.current.last=nowMs;
      perfRef.current.avg=perfRef.current.avg*0.9 + dt*0.1; perfRef.current.frames++;
      if(perfRef.current.frames%30===0){
        if(perfRef.current.avg>20 && scaleRef.current>0.6){ scaleRef.current=Math.max(0.6, scaleRef.current*0.88); }
        if(perfRef.current.avg<14 && scaleRef.current<1.25){ scaleRef.current=Math.min(1.25, scaleRef.current*1.05); }
        const dpr=Math.min(window.devicePixelRatio||1,2);
        const w=Math.floor(c.clientWidth*dpr*scaleRef.current); const h=Math.floor(c.clientHeight*dpr*scaleRef.current);
        if(c.width!==w||c.height!==h){ c.width=w; c.height=h; gl.viewport(0,0,w,h); }
      }

      const w=c.width,h=c.height; gl.uniform2f(uRes,w,h);
      const t=(nowMs-startRef.current)/1000; gl.uniform1f(uTime,t);
      gl.uniform2f(uMouse,mouse.x,mouse.y); gl.uniform1f(uSeed,seedRef.current);
      const q = (scaleRef.current-0.6)/(1.25-0.6); const qn=Math.max(0.0, Math.min(1.0, q)); gl.uniform1f(uQ, qn);

      if(t>=nextAutoAtRef.current){
        const ax=(Math.random()-0.5)*(w/h); const ay=(Math.random()-0.5);
        blastsRef.current=[...blastsRef.current,{x:ax,y:ay,t:t}].slice(-6);
        playBlastSound(ax,ay,0.6*q+0.25); playHeartbeatSound(ax,ay,0.6*q+0.25);
        nextAutoAtRef.current=t+1.0+Math.random()*2.0 + (1.5-1.0*q);
      }

      const MAXB=6; const barr=blastBufRef.current; let bcount=0; const bnext=[];
      for(let i=0;i<blastsRef.current.length;i++){const b=blastsRef.current[i]; if(t-b.t<5.5){if(bcount<MAXB){barr[bcount*4+0]=b.x;barr[bcount*4+1]=b.y;barr[bcount*4+2]=b.t;barr[bcount*4+3]=0.0;bcount++;} bnext.push(b);} }
      blastsRef.current=bnext; gl.uniform1i(uBlastCount,bcount); if(bcount>0) gl.uniform4fv(uBlasts,barr.subarray(0,bcount*4));

      const MAXC=16; const carr=cutBufRef.current; let ccount=0; const cnext=[];
      const ccap=Math.max(4, Math.floor(4+8*qn));
      for(let i=0;i<cutsRef.current.length&&ccount<ccap;i++){ const k=cutsRef.current[i]; if(t-k.t<2.6){ carr[ccount*4+0]=k.x; carr[ccount*4+1]=k.y; carr[ccount*4+2]=k.t; carr[ccount*4+3]=k.a; ccount++; cnext.push(k);} }
      cutsRef.current=cnext; gl.uniform1i(uCutCount,ccount); if(ccount>0) gl.uniform4fv(uCuts,carr.subarray(0,ccount*4));

      const MAXL=16; const larr=boltBufRef.current; let lcount=0; const lnext=[];
      const lcap=Math.max(2, Math.floor(2+10*qn));
      for(let i=0;i<boltsRef.current.length;i++){ const k=boltsRef.current[i]; if(t-k.t<1.8){ if(lcount<MAXL && lcount<lcap){ larr[lcount*4+0]=k.x; larr[lcount*4+1]=k.y; larr[lcount*4+2]=k.t; larr[lcount*4+3]=k.a; lcount++; } lnext.push(k);} }
      boltsRef.current=lnext; gl.uniform1i(uBoltCount,lcount); if(lcount>0) gl.uniform4fv(uBolts,larr.subarray(0,lcount*4));

      gl.drawArrays(gl.TRIANGLES,0,6);
      rafRef.current=requestAnimationFrame(draw);
    };
    drawRef.current=draw; rafRef.current=requestAnimationFrame(draw);

    return ()=>{
      if(rafRef.current) cancelAnimationFrame(rafRef.current);
      c.removeEventListener("pointermove",onMove); c.removeEventListener("pointerdown",onDown); window.removeEventListener("pointerup",endDrag); c.removeEventListener("pointercancel",endDrag); c.removeEventListener("click",onClick);
      const pr=progRef.current; if(pr){ gl.deleteProgram(pr); progRef.current=null; }
      const vs=vsRef.current; if(vs){ gl.deleteShader(vs); vsRef.current=null; }
      const fs=fsRef.current; if(fs){ gl.deleteShader(fs); fsRef.current=null; }
      if(bufRef.current){gl.deleteBuffer(bufRef.current);bufRef.current=null;}
      stopTearSound();
      ro.disconnect();
    };
  },[]);

  return (
    <div className="relative w-full h-screen bg-black">
      <canvas ref={canvasRef} className="absolute inset-0 w-full h-full block" />
      <div className="pointer-events-auto absolute top-4 left-4 flex gap-2 items-center">
        <Button variant="secondary" className="rounded-2xl shadow" onClick={randomize}>
          <Shuffle className="w-4 h-4 mr-2" /> Randomize
        </Button>
        <Button variant="secondary" className="rounded-2xl shadow" onClick={toggle}>
          {running ? <Pause className="w-4 h-4 mr-2" /> : <Play className="w-4 h-4 mr-2" />} {running ? "Pause" : "Play"}
        </Button>
        <Button variant="secondary" className="rounded-2xl shadow" onClick={()=>{const a=ensureAudio(); if(a){setMuted(m=>{const nm=!m; a.master.gain.setTargetAtTime(nm?0.0:0.9, a.ctx.currentTime, 0.02); return nm;});}}}>
          {muted ? <VolumeX className="w-4 h-4 mr-2" /> : <Volume2 className="w-4 h-4 mr-2" />} {muted ? "Muted" : "Sound"}
        </Button>
        <Button variant="secondary" className="rounded-2xl shadow" onClick={savePNG}>
          <Download className="w-4 h-4 mr-2" /> Save PNG
        </Button>
        <div className="ml-2 text-xs text-white/70 select-none hidden sm:block">
          Click/Drag to slice & lightning <Bomb className="inline w-4 h-4 ml-1" />
        </div>
      </div>
      <div className="absolute bottom-3 right-4 text-[10px] tracking-wide text-white/50 select-none">
        Creative coding • shock, glitch, flash • lightning tear audio
      </div>
    </div>
  );
}
