/*
Gull Author Vadim Demichev, gull is PUBLIC DOMAIN
https://chessprogramming.wikispaces.com/Vadim+Demichev
Vadim Demichev on public domain:
... Moreover, I believe that when someone releases an ENGINE as a public domain project it's a kind of 
invitation "feel free to improve it and make it commercial"! Since nothing prevented the original authors 
to place a restriction on commercial use of their code.
*/
#define TBPROBE // Only input code Syzygy, in recognition of Ronald de Man. 
/*
Copyright (c) 2011-2013 Ronald de Man
https://chessprogramming.wikispaces.com/Ronald+de+Man
This file may be redistributed and/or modified without restrictions.
Author code SYZYGY.
*/

// RANDOM MERSENNE TWISTER 
/*
A C-program for MT19937, with initialization improved 2002/1/26.
Coded by Takuji Nishimura and Makoto Matsumoto.
Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura, all rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, 
this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The names of its contributors may not be used to endorse or promote products derived from this 
software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Any feedback is very welcome.
http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
*/

/*
Modifications and code written by José M. Velasco declared how PUBLIC DOMAIN.
*/

#pragma comment(lib, "Advapi32.lib")
#pragma warning(disable: 589) // transfer of control bypasses initialization of: INTEL_COMPILER 

#define WINDOWS_X64 // Platform x64 and HNI if Off platform x32

#ifndef WINDOWS_X64
#define NTDDI_VERSION 0x05010200
#define _WIN32_WINNT 0x0501
#endif

#ifdef WINDOWS_X64
//#define HNI // AVX, BMI
#ifdef HNI
#define PLATFORM_ "avx64" // AVX magics + popcount POPCNT
#else
#define PLATFORM_ "x64" // POPCNT SSE42 and NO POPCNT
#endif
#else //WINDOWS_X32 POPCNT SSE42 and NO POPCNT
#define PLATFORM_ "x32"
#endif 

#define ENGINE "Gull."
#define VERSION_ "3.1_JV"
#define AUTOR "Author ThinkingALot, "
#define MODS "small things by Jose Velasco, "
#define CODES "used code of Ronald de Man (syzygy tablebases) "
#define CODES1 "and M. Matsumoto and T.Nishimura (Mersenne Twister)"

#define MERSENNE // Random mersenne twister
//#undef MERSENNE

#define CPU_TIMING
#undef CPU_TIMING

#define TUNER
#undef TUNER

#include <iostream>
#include "setjmp.h"
#include "windows.h"

#define EXPLAIN_EVAL
#undef EXPLAIN_EVAL

#ifdef WINDOWS_X64
#define LARGE_PAGES
//#undef LARGE_PAGES
#endif

#define MP_NPS
//#undef MP_NPS

#define TIME_TO_DEPTH
//#undef TIME_TO_DEPTH

#define TB
//#undef TB

using namespace std;

typedef unsigned char uint8;
typedef char sint8;
typedef unsigned short uint16;
typedef short sint16;
typedef unsigned int uint32;
typedef int sint32;
typedef unsigned long long uint64;
typedef long long sint64;

#define Convert(x,type) ((type)(x))

#define Abs(x) ((x) > 0 ? (x) : (-(x)))
#define Sgn(x) ((x) == 0 ? 0 : ((x) > 0 ? 1 : (-1)))
#define Min(x,y) ((x) < (y) ? (x) : (y))
#define Max(x,y) ((x) > (y) ? (x) : (y))
#define Sqr(x) ((x) * (x))
#define T(x) ((x) != 0)
#define F(x) ((x) == 0)
#define Even(x) F((x) & 1)
#define Odd(x) T((x) & 1)
#define Combine(x,y) ((x) | ((y) << 16))
#define Compose(x,y) ((x) + ((y) << 16))
#define Compose16(x,y) Compose((x)/16,(y)/16)
#define Compose64(x,y) Compose((x)/64,(y)/64)
#define Compose256(x,y) Compose((x)/256,(y)/256)
#define Opening(x) Convert((x) & 0xFFFF,sint16)
#define Endgame(x) ((((x) >> 15) & 1) + Convert((x) >> 16,sint16))

#define File(x) ((x) & 7)
#define Rank(x) ((x) >> 3)
#define CRank(me,x) ((me) ? (7 - Rank(x)) : Rank(x))
#define NDiag(x) (7 - File(x) + Rank(x))
#define SDiag(x) (File(x) + Rank(x))
#define Dist(x,y) Max(Abs(Rank(x)-Rank(y)),Abs(File(x)-File(y)))
#define VarC(var,me) ((me) ? (var##_b) : (var##_w))
#define PVarC(prefix,var,me) ((me) ? (prefix##.##var##_b) : (prefix##.##var##_w))

#define Bit(x) (Convert(1,uint64) << (x))
#ifndef HNI
#define Cut(x) (x &= (x) - 1)
#else
#define Cut(x) (x = _blsr_u64(x))
#endif
#define Multiple(x) T((x) & ((x) - 1))
#define Single(x) F((x) & ((x) - 1))
#define Add(x,b) (x |= Bit(b))

#define From(move) (((move) >> 6) & 0x3f)
#define To(move) ((move) & 0x3f)
#define SetScore(move,score) ((move) = (((move) & 0xFFFF) | ((score) << 16)))
#define BitFrom(move) Bit(From(move))
#define BitTo(move) Bit(To(move))
#define MakeMove(from,to) ((from) << 6) | (to))
#define MakeMoveF(from,to,flags) ((from) << 6) | (to) | (flags))
#define MakeMoveFS(from,to,flags,score) ((from) << 6) | (to) | (flags) | (score))
#define PieceAtOrigin(move) Square(From(move))
#define Target(move) Square(To(move)) 

#define Empty Convert(0,uint64)
#define Filled (~Empty)
#define Interior Convert(0x007E7E7E7E7E7E00,uint64)
#define Boundary (~Interior)
#define WhiteArea Convert(0x00000000FFFFFFFF,uint64)
#define BlackArea (~WhiteArea)
#define LightArea Convert(0x55AA55AA55AA55AA,uint64)
#define DarkArea (~LightArea)
#define FileA Convert(0x0101010101010101,uint64)
#define Line0 Convert(0x00000000000000FF,uint64)

#define High32(x) ((x) >> 32)
#define Low32(x) Convert(x,uint32)

#define White 0
#define Black 1
#define WhitePawn 2
#define BlackPawn 3
#define WhiteKnight 4
#define BlackKnight 5
#define WhiteLight 6
#define BlackLight 7
#define WhiteDark 8
#define BlackDark 9
#define WhiteRook 10
#define BlackRook 11
#define WhiteQueen 12
#define BlackQueen 13
#define WhiteKing 14
#define BlackKing 15

#define IsSlider(x) T(0x3FC0 & Bit(x))

#define CanCastle_OO 1
#define CanCastle_oo 2
#define CanCastle_OOO 4
#define CanCastle_ooo 8

#define FlagCastling 0x1000
#define FlagEP 0x2000
#define FlagPKnight 0x4000
#define FlagPLight 0x6000
#define FlagPDark 0x8000
#define FlagPRook 0xA000
#define FlagPQueen 0xC000

#define IsPromotion(move) T((move) & 0xC000)
#define IsCastling(move) T((move) & 0x1000)
#define IsEP(move) (((move) & 0xF000) == 0x2000)
#define Promotion(move,side) ((side) + (((move) & 0xF000) >> 12))

const uint8 UpdateCastling[64] = {
	0xFF^CanCastle_OOO,0xFF,0xFF,0xFF,0xFF^(CanCastle_OO|CanCastle_OOO),0xFF,0xFF,0xFF^CanCastle_OO,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
	0xFF^CanCastle_ooo,0xFF,0xFF,0xFF,0xFF^(CanCastle_oo|CanCastle_ooo),0xFF,0xFF,0xFF^CanCastle_oo
};

const uint64 BMagic[64] = {
    0x0048610528020080, 0x00c4100212410004, 0x0004180181002010, 0x0004040188108502, 
    0x0012021008003040, 0x0002900420228000, 0x0080808410c00100, 0x000600410c500622, 
    0x00c0056084140184, 0x0080608816830050, 0x00a010050200b0c0, 0x0000510400800181, 
    0x0000431040064009, 0x0000008820890a06, 0x0050028488184008, 0x00214a0104068200, 
    0x004090100c080081, 0x000a002014012604, 0x0020402409002200, 0x008400c240128100, 
    0x0001000820084200, 0x0024c02201101144, 0x002401008088a800, 0x0003001045009000, 
    0x0084200040981549, 0x0001188120080100, 0x0048050048044300, 0x0008080000820012, 
    0x0001001181004003, 0x0090038000445000, 0x0010820800a21000, 0x0044010108210110, 
    0x0090241008204e30, 0x000c04204004c305, 0x0080804303300400, 0x00a0020080080080, 
    0x0000408020220200, 0x0000c08200010100, 0x0010008102022104, 0x0008148118008140, 
    0x0008080414809028, 0x0005031010004318, 0x0000603048001008, 0x0008012018000100, 
    0x0000202028802901, 0x004011004b049180, 0x0022240b42081400, 0x00c4840c00400020, 
    0x0084009219204000, 0x000080c802104000, 0x0002602201100282, 0x0002040821880020, 
    0x0002014008320080, 0x0002082078208004, 0x0009094800840082, 0x0020080200b1a010, 
    0x0003440407051000, 0x000000220e100440, 0x00480220a4041204, 0x00c1800011084800, 
    0x000008021020a200, 0x0000414128092100, 0x0000042002024200, 0x0002081204004200
};

const uint64 RMagic[64] = {
    0x00800011400080a6, 0x004000100120004e, 0x0080100008600082, 0x0080080016500080, 
    0x0080040008000280, 0x0080020005040080, 0x0080108046000100, 0x0080010000204080, 
    0x0010800424400082, 0x00004002c8201000, 0x000c802000100080, 0x00810010002100b8, 
    0x00ca808014000800, 0x0002002884900200, 0x0042002148041200, 0x00010000c200a100, 
    0x00008580004002a0, 0x0020004001403008, 0x0000820020411600, 0x0002120021401a00, 
    0x0024808044010800, 0x0022008100040080, 0x00004400094a8810, 0x0000020002814c21, 
    0x0011400280082080, 0x004a050e002080c0, 0x00101103002002c0, 0x0025020900201000, 
    0x0001001100042800, 0x0002008080022400, 0x000830440021081a, 0x0080004200010084, 
    0x00008000c9002104, 0x0090400081002900, 0x0080220082004010, 0x0001100101000820, 
    0x0000080011001500, 0x0010020080800400, 0x0034010224009048, 0x0002208412000841, 
    0x000040008020800c, 0x001000c460094000, 0x0020006101330040, 0x0000a30010010028, 
    0x0004080004008080, 0x0024000201004040, 0x0000300802440041, 0x00120400c08a0011, 
    0x0080006085004100, 0x0028600040100040, 0x00a0082110018080, 0x0010184200221200, 
    0x0040080005001100, 0x0004200440104801, 0x0080800900220080, 0x000a01140081c200, 
    0x0080044180110021, 0x0008804001001225, 0x00a00c4020010011, 0x00001000a0050009, 
    0x0011001800021025, 0x00c9000400620811, 0x0032009001080224, 0x001400810044086a
};

const int BShift[64] = {
    58, 59, 59, 59, 59, 59, 59, 58, 
	59, 59, 59, 59, 59, 59, 59, 59,
    59, 59, 57, 57, 57, 57, 59, 59, 
	59, 59, 57, 55, 55, 57, 59, 59,
    59, 59, 57, 55, 55, 57, 59, 59, 
	59, 59, 57, 57, 57, 57, 59, 59,
    59, 59, 59, 59, 59, 59, 59, 59, 
	58, 59, 59, 59, 59, 59, 59, 58
};

const int BOffset[64] = {
    0, 64, 96, 128, 160, 192, 224, 256, 
    320, 352, 384, 416, 448, 480, 512, 544, 
    576, 608, 640, 768, 896, 1024, 1152, 1184, 
    1216, 1248, 1280, 1408, 1920, 2432, 2560, 2592, 
    2624, 2656, 2688, 2816, 3328, 3840, 3968, 4000, 
    4032, 4064, 4096, 4224, 4352, 4480, 4608, 4640, 
    4672, 4704, 4736, 4768, 4800, 4832, 4864, 4896, 
    4928, 4992, 5024, 5056, 5088, 5120, 5152, 5184 
};

const int RShift[64] = {
    52, 53, 53, 53, 53, 53, 53, 52, 
	53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53, 
	53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53, 
	53, 54, 54, 54, 54, 54, 54, 53,
    53, 54, 54, 54, 54, 54, 54, 53, 
	52, 53, 53, 53, 53, 53, 53, 52
};

const int ROffset[64] = {
    5248, 9344, 11392, 13440, 15488, 17536, 19584, 21632, 
    25728, 27776, 28800, 29824, 30848, 31872, 32896, 33920, 
    35968, 38016, 39040, 40064, 41088, 42112, 43136, 44160, 
    46208, 48256, 49280, 50304, 51328, 52352, 53376, 54400, 
    56448, 58496, 59520, 60544, 61568, 62592, 63616, 64640, 
    66688, 68736, 69760, 70784, 71808, 72832, 73856, 74880, 
    76928, 78976, 80000, 81024, 82048, 83072, 84096, 85120, 
    87168, 91264, 93312, 95360, 97408, 99456, 101504, 103552
};
uint64 * BOffsetPointer[64];
uint64 * ROffsetPointer[64];

#ifndef HNI
#define BishopAttacks(sq,occ) (*(BOffsetPointer[sq] + (((BMagicMask[sq] & (occ)) * BMagic[sq]) >> BShift[sq])))
#define RookAttacks(sq,occ) (*(ROffsetPointer[sq] + (((RMagicMask[sq] & (occ)) * RMagic[sq]) >> RShift[sq])))
#else
#define BishopAttacks(sq,occ) (*(BOffsetPointer[sq] + _pext_u64(occ,BMagicMask[sq])))
#define RookAttacks(sq,occ) (*(ROffsetPointer[sq] + _pext_u64(occ,RMagicMask[sq])))
#endif
#define QueenAttacks(sq,occ) (BishopAttacks(sq,occ) | RookAttacks(sq,occ))

#define MatWQ 1
#define MatBQ 3
#define MatWR (3 * 3)
#define MatBR (3 * 3 * 3)
#define MatWL (3 * 3 * 3 * 3)
#define MatBL (3 * 3 * 3 * 3 * 2)
#define MatWD (3 * 3 * 3 * 3 * 2 * 2)
#define MatBD (3 * 3 * 3 * 3 * 2 * 2 * 2)
#define MatWN (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2)
#define MatBN (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3)
#define MatWP (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3 * 3)
#define MatBP (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3 * 3 * 9)
#define TotalMat ((2*(MatWQ+MatBQ)+MatWL+MatBL+MatWD+MatBD+2*(MatWR+MatBR+MatWN+MatBN)+8*(MatWP+MatBP)) + 1)
#define FlagUnusualMaterial (1 << 30)

const int MatCode[16] = {0,0,MatWP,MatBP,MatWN,MatBN,MatWL,MatBL,MatWD,MatBD,MatWR,MatBR,MatWQ,MatBQ,0,0};
const uint64 File[8] = {FileA,FileA<<1,FileA<<2,FileA<<3,FileA<<4,FileA<<5,FileA<<6,FileA<<7};
const uint64 Line[8] = {Line0,(Line0<<8),(Line0<<16),(Line0<<24),(Line0<<32),(Line0<<40),(Line0<<48),(Line0<<56)};

#define opp (1 ^ (me))

#define IPawn(me) (WhitePawn | (me))
#define IKnight(me) (WhiteKnight | (me))
#define ILight(me) (WhiteLight | (me))
#define IDark(me) (WhiteDark | (me))
#define IRook(me) (WhiteRook | (me))
#define IQueen(me) (WhiteQueen | (me))
#define IKing(me) (WhiteKing | (me))

#define BB(i) Board->bb[i]
#define Pawn(me) (BB(WhitePawn | (me)))
#define Knight(me) (BB(WhiteKnight | (me)))
#define Bishop(me) (BB(WhiteLight | (me)) | BB(WhiteDark | (me)))
#define Rook(me) (BB(WhiteRook | (me)))
#define Queen(me) (BB(WhiteQueen | (me)))
#define King(me) (BB(WhiteKing | (me)))
#define Piece(me) (BB(me))
#define NonPawn(me) (Piece(me) ^ Pawn(me))
#define NonPawnKing(me) (NonPawn(me) ^ King(me))
#define BSlider(me) (Bishop(me) | Queen(me))
#define RSlider(me) (Rook(me) | Queen(me))
#define Major(me) RSlider(me)
#define Minor(me) (Knight(me) | Bishop(me))
#define Slider(me) (BSlider(me) | RSlider(me))
#define PieceAll (Piece(White) | Piece(Black))
#define SliderAll (Slider(White) | Slider(Black))
#define PawnAll (Pawn(White) | Pawn(Black))
#define NonPawnKingAll (NonPawnKing(White) | NonPawnKing(Black))
#define KingPos(me) (lsb(King(me)))

#define ShiftNW(target) (((target) & (~(File[0] | Line[7]))) << 7)
#define ShiftNE(target) (((target) & (~(File[7] | Line[7]))) << 9)
#define ShiftSE(target) (((target) & (~(File[7] | Line[0]))) >> 7)
#define ShiftSW(target) (((target) & (~(File[0] | Line[0]))) >> 9)
#define ShiftW(me,target) ((me) ? ShiftSW(target) : ShiftNW(target))
#define ShiftE(me,target) ((me) ? ShiftSE(target) : ShiftNE(target))
#define ShiftN(target) ((target) << 8)
#define ShiftS(target) ((target) >> 8)
#define Shift(me,target) ((me) ? ShiftS(target) : ShiftN(target))
#define PushW(me) ((me) ? (-9) : (7))
#define PushE(me) ((me) ? (-7) : (9))
#define Push(me) ((me) ? (-8) : (8))
#define Dir(me) ((me) ? (-1) : (1))
#define IsGreater(me,x,y) ((me) ? ((x) < (y)) : ((x) > (y)))

#define Line(me,n) ((me) ? Line[7 - n] : Line[n])
#define Square(sq) Board->square[sq]
#define AddMove(from,to,flags,score) { *list = ((from) << 6) | (to) | (flags) | (score); list++; }
#define AddCapture(from,to,flags) AddMove(from,to,flags,MvvLva[Square(from)][Square(to)])
#define AddCaptureP(piece,from,to,flags) AddMove(from,to,flags,MvvLva[piece][Square(to)])
#define AddHistoryP(piece,from,to,flags) AddMove(from,to,flags,HistoryP(piece,from,to))
#define AddHistory(from,to) AddMove(from,to,0,History(from,to))
#define AddDeltaP(piece,from,to,flags) AddMove(from,to,flags,Convert(DeltaScore(piece,from,to)+(sint16)0x4000,int) << 16)
#define AddDelta(from,to) AddMove(from,to,0,Convert(Delta(from,to)+(sint16)0x4000,int) << 16)
#define AddCDeltaP(piece,from,to,flags) {if (DeltaScore(piece,from,to) >= Current->margin) AddMove(from,to,flags,Convert(DeltaScore(piece,from,to)+(sint16)0x4000,int) << 16)}
#define AddCDelta(from,to) {if (Delta(from,to) >= Current->margin) AddMove(from,to,0,Convert(Delta(from,to)+(sint16)0x4000,int) << 16)}

#define Check(me) T(Current->att[(me) ^ 1] & King(me))
#define IsIllegal(me,move) ((T(Current->xray[opp] & Bit(From(move))) && F(Bit(To(move)) & FullLine[lsb(King(me))][From(move)])) \
	|| (IsEP(move) && T(Line[Rank(From(move))] & King(me)) && T(Line[Rank(From(move))] & Major(opp)) && \
	T(RookAttacks(lsb(King(me)),PieceAll ^ Bit(From(move)) ^ Bit(Current->ep_square - Push(me))) & Major(opp))))
#define IsRepetition(margin,move) ((margin) > 0 && Current->ply >= 2 && (Current-1)->move == ((To(move) << 6) | From(move)) && F(Square(To(move))) && F((move) & 0xF000))

#define IncV(var,x) (me ? (var -= (x)) : (var += (x)))

#define DecV(var,x) IncV(var,-(x))

#define KpkValue 300

//#define EvalValue 30000
#define EvalValue 26000
#define WdlValue 28000
#define DtzValue 29000
#define MateValue 32760
#define MateScore (MateValue - 128)

/* 
general move:
0 - 11: from & to
12 - 15: flags
16 - 23: history
24 - 25: spectial moves: killers, refutations...
26 - 30: MvvLva
delta move:
0 - 11: from & to
12 - 15: flags
16 - 31: sint16 delta + (sint16)0x4000
*/
const int MvvLvaVictim[16] = {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3}; 
const int MvvLvaAttacker[16] = {0, 0, 5, 5, 4, 4, 3, 3, 3, 3, 2, 2, 1, 1, 6, 6};
const int MvvLvaAttackerKB[16] = {0, 0, 9, 9, 7, 7, 5, 5, 5, 5, 3, 3, 1, 1, 11, 11};
#define PawnCaptureMvvLva(attacker) (MvvLvaAttacker[attacker])
#define MaxPawnCaptureMvvLva (MvvLvaAttacker[15]) // 6
#define KnightCaptureMvvLva(attacker) (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[attacker]) 
#define MaxKnightCaptureMvvLva (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[15]) // 17
#define BishopCaptureMvvLva(attacker) (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[attacker] + 1)
#define MaxBishopCaptureMvvLva (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[15] + 1) // 18
#define RookCaptureMvvLva(attacker) (MaxBishopCaptureMvvLva + MvvLvaAttacker[attacker])
#define MaxRookCaptureMvvLva (MaxBishopCaptureMvvLva + MvvLvaAttacker[15]) // 24
#define QueenCaptureMvvLva(attacker) (MaxRookCaptureMvvLva + MvvLvaAttacker[attacker])

#define MvvLvaPromotion (MvvLva[WhiteQueen][BlackQueen])
#define MvvLvaPromotionKnight (MvvLva[WhiteKnight][BlackKnight])
#define MvvLvaPromotionCap(capture) (MvvLva[((capture) < WhiteRook) ? WhiteRook : ((capture) >= WhiteQueen ? WhiteKing : WhiteKnight)][BlackQueen])
#define MvvLvaPromotionKnightCap(capture) (MvvLva[WhiteKing][capture])
#define MvvLvaXray (MvvLva[WhiteQueen][WhitePawn])
#define MvvLvaXrayCap(capture) (MvvLva[WhiteKing][capture])
#define RefOneScore ((0xFF << 16) | (3 << 24))
#define RefTwoScore ((0xFF << 16) | (2 << 24))
#define KillerOneScore ((0xFF << 16) | (1 << 24))
#define KillerTwoScore (0xFF << 16)

#define halt_check if ((Current - Data) >= 126) {evaluate(); return Current->score;} \
    if (Current->ply >= 100) return 0; \
	for (i = 4; i <= Current->ply; i+= 2) if (Stack[sp-i] == Current->key) return 0
#define ExtFlag(ext) ((ext) << 16)
#define Ext(flags) (((flags) >> 16) & 0xF)
#define FlagHashCheck (1 << 20) // first 20 bits are reserved for the hash killer and extension
#define FlagHaltCheck (1 << 21)
#define FlagCallEvaluation (1 << 22)
#define FlagDisableNull (1 << 23)
#define FlagNeatSearch (FlagHashCheck | FlagHaltCheck | FlagCallEvaluation)
#define FlagNoKillerUpdate (1 << 24)
#define FlagReturnBestMove (1 << 25)

#define MSBZ(x) ((x) ? msb(x) : 63)
#define LSBZ(x) ((x) ? lsb(x) : 0)
#define NB(me, x) ((me) ? msb(x) : lsb(x))
#define NBZ(me, x) ((me) ? MSBZ(x) : LSBZ(x))

typedef struct {
	uint64 bb[16];
	uint8 square[64];
} GBoard;
__declspec(align(64)) GBoard Board[1];
uint64 Stack[2048];
int sp, save_sp;
uint64 nodes, check_node, check_node_smp;
GBoard SaveBoard[1];

typedef struct {
	uint64 key, pawn_key;
	uint16 move;
	uint8 turn, castle_flags, ply, ep_square, piece, capture;
	uint8 square[64];
	int pst, material;
} GPosData;
typedef struct {
	uint64 key, pawn_key, eval_key, att[2], patt[2], passer, xray[2], pin[2], threat, mask;
	uint8 turn, castle_flags, ply, ep_square, capture, gen_flags, piece, stage, mul, dummy, piece_nb;
	sint16 score;
	uint16 move, killer[3], ref[2];
	int best;
	int material, pst;
	int margin, *start, *current;
	int moves[230];
} GData;
__declspec(align(64)) GData Data[128];
GData *Current = Data;
#define FlagSort (1 << 0)
#define FlagNoBcSort (1 << 1)
GData SaveData[1];

enum {
	stage_search, s_hash_move, s_good_cap, s_special, s_quiet, s_bad_cap, s_none,
	stage_evasion, e_hash_move, e_ev, e_none, 
	stage_razoring, r_hash_move, r_cap, r_checks, r_none
};
#define StageNone ((1 << s_none) | (1 << e_none) | (1 << r_none))

typedef struct {
    uint32 key;
	uint16 date;
	uint16 move;
	sint16 low;
	sint16 high;
	uint16 flags;
	uint8 low_depth;
	uint8 high_depth;
} GEntry;

#define initial_hash_size (1024 * 1024)
sint64 hash_size = initial_hash_size;
uint64 hash_mask = (initial_hash_size - 4);
GEntry * Hash;

typedef struct {
	uint64 key;
	sint16 shelter[2];
	uint8 passer[2], draw[2];
	int score;
} GPawnEntry;

#define pawn_hash_size (1024 * 1024)
__declspec(align(64)) GPawnEntry PawnHash[pawn_hash_size];

#define pawn_hash_mask (pawn_hash_size - 1)

typedef struct {
	uint32 key;
	uint16 date;
	uint16 move;
	sint16 value;
	sint16 exclusion;
	uint8 depth;
	uint8 ex_depth;
	int knodes;
	int ply;
} GPVEntry;

#define pv_hash_size (1024 * 1024)
#define pv_cluster_size 4
#define pv_hash_mask (pv_hash_size - pv_cluster_size)
GPVEntry * PVHash = NULL;

int RootList[256];

#define prefetch(a,mode) _mm_prefetch(a,mode)

uint64 Forward[2][8];
uint64 West[8];
uint64 East[8];
uint64 PIsolated[8];
uint64 HLine[64];
uint64 VLine[64];
uint64 NDiag[64];
uint64 SDiag[64];
uint64 RMask[64];
uint64 BMask[64];
uint64 QMask[64];
uint64 BMagicMask[64];
uint64 RMagicMask[64];
uint64 NAtt[64];
uint64 SArea[64];
uint64 DArea[64];
uint64 NArea[64];
uint64 BishopForward[2][64];
uint64 PAtt[2][64];
uint64 PMove[2][64];
uint64 PWay[2][64];
uint64 PSupport[2][64];
uint64 Between[64][64];
uint64 FullLine[64][64];

#define magic_size 107648
uint64 * MagicAttacks;

typedef struct {
	sint16 score;
	uint8 phase, flags;
	uint8 mul[2], pieces[2];
} GMaterial;

GMaterial * Material;
#define FlagSingleBishop_w (1 << 0)
#define FlagSingleBishop_b (1 << 1)
#define FlagCallEvalEndgame_w (1 << 2)
#define FlagCallEvalEndgame_b (1 << 3)


int Pst[16 * 64];

#define Pst(piece,sq) Pst[((piece) << 6) | (sq)]
int MvvLva[16][16]; // [piece][capture]
uint64 TurnKey;
uint64 PieceKey[16][64];
uint64 CastleKey[16];
uint64 EPKey[8];
uint16 date;

uint64 Kpk[2][64][64]; 


sint16 History[16 * 64]; 

#define HistoryScore(piece,from,to) History[((piece) << 6) | (to)]
#define HistoryP(piece,from,to) ((Convert(HistoryScore(piece,from,to) & 0xFF00,int)/Convert(HistoryScore(piece,from,to) & 0x00FF,int)) << 16)
#define History(from,to) HistoryP(Square(from),from,to)
#define HistoryM(move) HistoryScore(Square(From(move)),From(move),To(move))
#define HistoryInc(depth) Min(((depth) >> 1) * ((depth) >> 1), 64)
#define HistoryGood(move) if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth)) \
	HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + ((HistoryInc(depth) << 8) | HistoryInc(depth)); \
	else HistoryM(move) += ((HistoryInc(depth) << 8) | HistoryInc(depth))
#define HistoryBad(move) if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth)) \
	HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + HistoryInc(depth); else HistoryM(move) += HistoryInc(depth)


sint16 Delta[16 * 4096];

#define DeltaScore(piece,from,to) Delta[((piece) << 12) | ((from) << 6) | (to)]
#define Delta(from,to) DeltaScore(Square(from),from,to)
#define DeltaM(move) Delta(From(move),To(move))
#define UpdateDelta if (F(Current->capture) && T(Current->move) && F(Current->move & 0xE000) && Current > Data) { \
	if (DeltaScore(Current->piece,From(Current->move),To(Current->move)) <= -Current->score - ((Current - 1)->score)) \
	DeltaScore(Current->piece,From(Current->move),To(Current->move)) = -Current->score - ((Current - 1)->score); \
	else DeltaScore(Current->piece,From(Current->move),To(Current->move))--; }
#define DeltaMarginP(piece,from,to) (DeltaScore(piece,from,to) >= Current->margin)
#define DeltaMargin(from,to) (Delta(from,to) >= Current->margin)

typedef struct {
	uint16 ref[2];
	uint16 check_ref[2];
} GRef;

GRef Ref[16 * 64];

#define RefPointer(piece,from,to) Ref[((piece) << 6) | (to)]
#define RefM(move) RefPointer(Square(To(move)),From(move),To(move))
#define UpdateRef(ref_move) if (T(Current->move) && RefM(Current->move).ref[0] != (ref_move)) { \
	RefM(Current->move).ref[1] = RefM(Current->move).ref[0]; RefM(Current->move).ref[0] = (ref_move); }
#define UpdateCheckRef(ref_move) if (T(Current->move) && RefM(Current->move).check_ref[0] != (ref_move)) { \
	RefM(Current->move).check_ref[1] = RefM(Current->move).check_ref[0]; RefM(Current->move).check_ref[0] = (ref_move); }

uint64 seed = 1;
uint8 PieceFromChar[256];
uint16 PV[128];
char info_string[1024];
char pv_string[1024];
char score_string[16];
char mstring[65536];
int MultiPV[256];
int pvp;
int pv_length;
int best_move, best_score;
int TimeLimit1, TimeLimit2, Console, HardwarePopCnt;
int DepthLimit, LastDepth, LastTime, LastValue, LastExactValue, PrevMove, InstCnt;
sint64 LastSpeed;
int PVN, Stop, Print, Input = 1, PVHashing = 1, Infinite, MoveTime, SearchMoves, SMPointer, Ponder, Searching, Previous;
#ifdef TB
int useDTZ, useWDL;
#endif
typedef struct {
	int Bad, Change, Singular, Early, FailLow, FailHigh;
} GSearchInfo;
GSearchInfo CurrentSI[1], BaseSI[1];
#ifdef CPU_TIMING
int CpuTiming = 0, UciMaxDepth = 0, UciMaxKNodes = 0, UciBaseTime = 1000, UciIncTime = 5;
int GlobalTime[2] = { 0, 0 };
int GlobalInc[2] = { 0, 0 };
int GlobalTurn = 0;
#define CyclesPerMSec Convert(3400000, sint64)
#endif
int Aspiration = 1, LargePages = 1;
#define TimeSingTwoMargin 20
#define TimeSingOneMargin 30
#define TimeNoPVSCOMargin 60
#define TimeNoChangeMargin 70
#define TimeRatio 120
#define PonderRatio 120
#define MovesTg 30
#define InfoLag 5000
#define InfoDelay 1000
sint64 StartTime, InfoTime, CurrTime;
uint16 SMoves[256];

jmp_buf Jump, ResetJump;
HANDLE StreamHandle; 

#define ExclSingle(depth) 8
#define ExclDouble(depth) 16
#define ExclSinglePV(depth) 8
#define ExclDoublePV(depth) 16

// EVAL

const sint8 DistC[8] = {3, 2, 1, 0, 0, 1, 2, 3};
const sint8 RankR[8] = {-3, -2, -1, 0, 1, 2, 3, 4};

const int SeeValue[16] = {0, 0, 90, 90, 325, 325, 325, 325, 325, 325, 510, 510, 975, 975, 30000, 30000};
const int PieceType[16] = {0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5};


#define V(x) (x)


#define ArrayIndex(width,row,column) (((row) * (width)) + (column))
#define Av(x,width,row,column) (*((x) + ArrayIndex(width,row,column)))
#define TrAv(x,w,r,c) Av(x,0,0,(((r)*(2*(w)-(r)+1))/2)+(c))
#define Sa(x,y) Av(x,0,0,y)
#define Ca(x,y) Compose(Av(x,0,0,((y) * 2)),Av(x,0,0,((y) * 2)+1))

// EVAL WEIGHTS

// tuner: start
enum { // tuner: enum
	IMatLinear,
	IMatQuadMe = IMatLinear + 5,
	IMatQuadOpp = IMatQuadMe + 14,
	IBishopPairQuad = IMatQuadOpp + 10,
	IMatSpecial = IBishopPairQuad + 9,
	IPstQuadWeights = IMatSpecial + 20,
	IPstLinearWeights = IPstQuadWeights + 48,
	IPstQuadMixedWeights = IPstLinearWeights + 48,
	IMobilityLinear = IPstQuadMixedWeights + 24,
	IMobilityLog = IMobilityLinear + 8,
	IShelterValue = IMobilityLog + 8,
	IStormQuad = IShelterValue + 15,
	IStormLinear = IStormQuad + 5,
	IStormHof = IStormLinear + 5,
	IPasserQuad = IStormHof + 2,
	IPasserLinear = IPasserQuad + 18,
	IPasserAttDefQuad = IPasserLinear + 18,
	IPasserAttDefLinear = IPasserAttDefQuad + 4,
	IPasserSpecial = IPasserAttDefLinear + 4,
	IIsolated = IPasserSpecial + 4,
	IUnprotected = IIsolated + 10,
	IBackward = IUnprotected + 6,
	IDoubled = IBackward + 4,
	IRookSpecial = IDoubled + 4,
	ITactical = IRookSpecial + 20,
	IKingDefence = ITactical + 12,
	IPawnSpecial = IKingDefence + 8,
	IBishopSpecial = IPawnSpecial + 8,
	IKnightSpecial = IBishopSpecial + 4,
	IPin = IKnightSpecial + 10,
	IKingRay = IPin + 10,
	IKingAttackWeight = IKingRay + 6
};

const int Phase[5] = {
	0, 325, 325, 510, 975
};
#define MaxPhase (16 * Phase[0] + 4 * Phase[1] + 4 * Phase[2] + 4 * Phase[3] + 2 * Phase[4])
#define PhaseMin (2 * Phase[3] + Phase[1] + Phase[2])
#define PhaseMax (MaxPhase - Phase[1] - Phase[2])

const int MatLinear[5] = { // tuner: type=array, var=50, active=0
	3, 0, 3, 19, 0
};
// pawn, knight, bishop, rook, queen
const int MatQuadMe[14] = { // tuner: type=array, var=1000, active=0
	-33, 17, -23, -155, -247,
	15, 296, -105, -83,
	-162, 327, 315,
	-861, -1013
};
const int MatQuadOpp[10] = { // tuner: type=array, var=1000, active=0
	-14, 47, -20, -278,
	35, 39, 49,
	9, -2,
	75
};
const int BishopPairQuad[9] = { // tuner: type=array, var=1000, active=0
	-38, 164, 99, 246, -84, -57, -184, 88, -186
};

enum { MatRB, MatRN, MatQRR, MatQRB, MatQRN, MatQ3, MatBBR, MatBNR, MatNNR, MatM };
const int MatSpecial[20] = { // tuner: type=array, var=30, active=0
	13, -13, 10, -9, 8, 12, 4, 6, 5, 9, -3, -8, -4, 7, 2, 0, 0, -6, 1, 3
};

// piece type (6) * direction (4: h center dist, v center dist, diag dist, rank) * phase (2)
const int PstQuadWeights[48] = { // tuner: type=array, var=100, active=0
	-15, -19, -70, -13, 33, -20, 0, 197, -36, -122, 0, -60, -8, -3, -17, -28,
	-27, -63, -17, -7, 14, 0, -24, -5, -64, -2, 0, -38, -8, 0, 77, 11,
	-67, 3, -4, -92, -2, 12, -13, -42, -62, -84, -175, -42, -2, -17, 40, -19
};
const int PstLinearWeights[48] = { // tuner: type=array, var=500, active=0
	-107, 67, -115, 83, -55, 67, 92, 443, -177, 5, -82, -61, -106, -104, 273, 130,
	0, -145, -105, -58, -99, -37, -133, 14, -185, -43, -67, -53, 53, -65, 174, 134,
	-129, 7, 98, -231, 107, -40, -27, 311, 256, -117, 813, -181, 2, -215, -44, 344
};
// piece type (6) * type (2: h * v, h * rank) * phase (2)
const int PstQuadMixedWeights[24] = { // tuner: type=array, var=100, active=0
	14, -6, 1, -4, -8, -2, 4, -4,
	1, -7, -12, 0, -2, -1, -5, 4,
	5, -10, 0, 4, -2, 5, 4, -2
};
// piece type (4) * phase (2)
const int MobilityLinear[8] = { // tuner: type=array, var=300, active=0
	328, 171, 311, 102, 284, 164, 155, 288
};
const int MobilityLog[8] = { // tuner: type=array, var=500, active=0
	485, -21, 388, 389, -168, 313, 438, -276
};
int Mobility[4][32];

// file type (3) * distance from 2d rank/open (5)
const int ShelterValue[15] = {  // tuner: type=array, var=10, active=0
	2, 9, 11, 0, 0, 12, 18, 11, 0, 2, 24, 7, 8, 0, 0
};
sint16 Shelter[3][8];

enum { StormBlockedMul, StormShelterAttMul, StormConnectedMul, StormOpenMul, StormFreeMul };
const int StormQuad[5] = { // tuner: type=array, var=250, active=0
	126, 328, 463, 215, 89
};
const int StormLinear[5] = { // tuner: type=array, var=500, active=0
	83, 156, 438, 321, 12
};
enum { StormHofValue, StormOfValue };
const int StormHof[2] = { // tuner: type=array, var=20, active=1
	0, 22
};
sint16 StormBlocked[4];
sint16 StormShelterAtt[4];
sint16 StormConnected[4];
sint16 StormOpen[4];
sint16 StormFree[4];

// type (7: general, blocked, free, supported, protected, connected, outside, candidate, clear) * phase (2)
const int PasserQuad[18] = { // tuner: type=array, var=50, active=0
	19, 13, 21, 3, -24, 126, 0, 65, 32, 56, 27, -5, 32, -16, 13, 4, 1, 1
};
const int PasserLinear[18] = { // tuner: type=array, var=200, active=0
	41, 2, 111, 86, 178, 113, 202, 15, -61, 21, 93, 166, 86, 92, 27, 34, -18, -7
};
// type (2: att, def) * scaling (2: linear, log) 
const int PasserAttDefQuad[4] = { // tuner: type=array, var=500, active=0
	191, 51, 83, 19
};
const int PasserAttDefLinear[4] = { // tuner: type=array, var=500, active=0
	634, 4, 233, 66
};
enum { PasserOnePiece, PasserOpKingControl, PasserOpMinorControl, PasserOpRookBlock };
const int PasserSpecial[4] = { // tuner: type=array, var=100, active=0
	0, 0, 0, 13
};

uint8 LogDist[16];
int PasserGeneral[8];
int PasserBlocked[8];
int PasserFree[8];
int PasserSupported[8];
int PasserProtected[8];
int PasserConnected[8];
int PasserOutside[8];
int PasserCandidate[8];
int PasserClear[8];
sint16 PasserAtt[8];
sint16 PasserDef[8];
sint16 PasserAttLog[8];
sint16 PasserDefLog[8];

enum { IsolatedOpen, IsolatedClosed, IsolatedBlocked, IsolatedDoubledOpen, IsolatedDoubledClosed };
const int Isolated[10] = { // tuner: type=array, var=10, active=0
	6, 6, 8, 2, -8, 0, -1, 10, 7, 9
};
enum { UpBlocked, PasserTarget, ChainRoot };
const int Unprotected[6] = { // tuner: type=array, var=10, active=0
	4, 5, -5, -1, 9, -1
};
enum { BackwardOpen, BackwardClosed };
const int Backward[4] = { // tuner: type=array, var=10, active=0
	17, 10, 4, 1
};
enum { DoubledOpen, DoubledClosed };
const int Doubled[4] = { // tuner: type=array, var=10, active=0
	3, 0, 1, 0
};

enum { RookHof, RookHofWeakPAtt, RookOf, RookOfOpen, RookOfMinorFixed, RookOfMinorHaging, RookOfKingAtt, Rook7th, Rook7thK8th, Rook7thDoubled };
const int RookSpecial[20] = { // tuner: type=array, var=10, active=0
	8, 0, 2, 0, 11, 8, -1, 2, -1, -1, 14, -1, 5, -5, -5, 0, -6, 8, -7, 31
};

enum { TacticalMajorPawn, TacticalMinorPawn, TacticalMajorMinor, TacticalMinorMinor, TacticalThreat, TacticalDoubleThreat };
const int Tactical[12] = { // tuner: type=array, var=20, active=0
	-1, 5, 0, 5, 11, 29, 23, 32, 19, 11, 41, 12
};

enum { KingDefKnight, KingDefBishop, KingDefRook, KingDefQueen };
const int KingDefence[8] = { // tuner: type=array, var=5, active=0
	2, 0, 0, 1, 0, 0, 4, 0
};

enum { PawnChainLinear, PawnChain, PawnBlocked, PawnFileSpan };
const int PawnSpecial[8] = { // tuner: type=array, var=10, active=0
	11, 9, 9, 4, 0, 9, 1, 1
};

enum { BishopNonForwardPawn, BishopPawnBlock };
const int BishopSpecial[4] = { // tuner: type=array, var=5, active=0
	0, 0, 0, 3
};

const uint64 Outpost[2] = { Convert(0x00007E7E3C000000, uint64), Convert(0x0000003C7E7E0000, uint64) };
enum { KnightOutpost, KnightOutpostProtected, KnightOutpostPawnAtt, KnightOutpostBishopAtt, KnightOutpostKingAtt };
const int KnightSpecial[10] = { // tuner: type=array, var=10, active=0
	11, 7, 23, 0, 13, 6, 1, 5, 26, 6
};

enum { WeakPin, StrongPin, ThreatPin, SelfPawnPin, SelfPiecePin };
const int Pin[10] = { // tuner: type=array, var=20, active=0
	21, 39, 6, 80, 45, 29, 8, 9, 48, 27
};

enum { QKingRay, RKingRay, BKingRay };
const int KingRay[6] = { // tuner: type=array, var=20, active=0
	4, 8, -4, 11, 11, -3
};

const int KingAttackWeight[7] = { // tuner: type=array, var=20, active=0
	17, 14, 22, 45, 48, 64, 64
};
#define KingNAttack Compose(1, Av(KingAttackWeight, 0, 0, 0))
#define KingBAttack Compose(1, Av(KingAttackWeight, 0, 0, 1))
#define KingRAttack Compose(1, Av(KingAttackWeight, 0, 0, 2))
#define KingQAttack Compose(1, Av(KingAttackWeight, 0, 0, 3))
#define KingAttack Compose(1, 0)
#define KingAttackSquare Av(KingAttackWeight, 0, 0, 4)
#define KingNoMoves Av(KingAttackWeight, 0, 0, 5)
#define KingShelterQuad Av(KingAttackWeight, 0, 0, 6)

const int KingAttackScale[16] = { 0, 1, 4, 9, 16, 25, 36, 49, 64, 64, 64, 64, 64, 64, 64, 64 };
// tuner: stop

// END EVAL WEIGHTS

// SMP

#define MaxPrN 1
#ifndef DEBUG
#undef MaxPrN
#ifdef WINDOWS_X64
#define MaxPrN 64 // mustn't exceed 64
#else // WIN_X32
#define MaxPrN 32 // mustn't exceed 32
#endif // WINDOWS_X64
#endif


int PrN = 1, CPUs = 1, HT = 0, parent = 1, child = 0, WinParId, Id = 0, ResetHash = 1, NewPrN = 0, probedepth = 1;
HANDLE ChildPr[MaxPrN];
#define SplitDepth 10
#define SplitDepthPV 4
#define MaxSplitPoints 64 // mustn't exceed 64

typedef struct {
	GPosData Position[1];
	uint64 stack[100];
	uint16 killer[16][2];
	int sp, date;
} GPos;

#define FlagClaimed (1 << 1)
#define FlagFinished (1 << 2)

typedef struct {
	volatile uint16 move;
	volatile uint8 reduced_depth, research_depth, stage, ext, id, flags;
} GMove;

typedef struct {
	volatile LONG lock;
	volatile int claimed, active, finished, pv, move_number, current, depth, alpha, beta, singular, split, best_move, height; 
	GMove move[128];
	jmp_buf jump;
	GPos Pos[1];
} GSP;

typedef struct {
	volatile long long nodes, tb_hits, active_sp, searching;
#ifdef WINDOWS_X64	
	volatile long long stop, fail_high;
#else // WIN_X32
	volatile long stop, fail_high;
#endif //  WINDOWS_X64
	volatile sint64 hash_size;
	volatile int PrN;
	GSP Sp[MaxSplitPoints];
#ifdef TB
#ifdef WINDOWS_X64
	volatile long long tb_reload;
#else // WIN_X32
	volatile long tb_reload;
#endif //  WINDOWS_X64
	char SyzygyPath[1024];

	volatile int WdlPieces, DtzPieces;
#endif
} GSMPI;

#define SharedMaterialOffset (sizeof(GSMPI))
#define SharedMagicOffset (SharedMaterialOffset + TotalMat * sizeof(GMaterial))
#define SharedPVHashOffset (SharedMagicOffset + magic_size * sizeof(uint64))

GSMPI * Smpi;

jmp_buf CheckJump;

HANDLE SHARED = NULL, HASH = NULL;

#ifdef WINDOWS_X64
#define SET_BIT(var,bit) (InterlockedOr(&(var),1 << (bit)))
#define SET_BIT_64(var,bit) (InterlockedOr64(&(var),Bit(bit)));
#define ZERO_BIT_64(var,bit) (InterlockedAnd64(&(var),~Bit(bit)));
#define TEST_RESET_BIT(var,bit) (InterlockedBitTestAndReset64(&(var),bit))
#define TEST_RESET(var) (InterlockedExchange64(&(var),0))
#else // WIN_X32
#define SET_BIT(var,bit) (_InterlockedOr(&(var),1 << (bit)))
#define SET_BIT_64(var,bit) {if ((bit) < 32) _InterlockedOr((LONG*)&(var),1 << (bit)); else _InterlockedOr(((LONG*)(&(var))) + 1,1 << ((bit) - 32));}
#define ZERO_BIT_64(var,bit) {if ((bit) < 32) _InterlockedAnd((LONG*)&(var),~(1 << (bit))); else _InterlockedAnd(((LONG*)(&(var))) + 1,~(1 << ((bit) - 32)));}
#define TEST_RESET_BIT(var,bit) (InterlockedBitTestAndReset(&(var),bit))
#define TEST_RESET(var) (InterlockedExchange(&(var),0))
#endif
#define SET(var,value) (InterlockedExchange(&(var),value))

#define LOCK(lock) {while (InterlockedCompareExchange(&(lock),1,0)) _mm_pause();}
#define UNLOCK(lock) {SET(lock,0);}

// END SMP

__forceinline int lsb(uint64 x);
__forceinline int msb(uint64 x);
__forceinline int popcnt(uint64 x);
__forceinline int MinF(int x, int y);
__forceinline int MaxF(int x, int y);
__forceinline double MinF(double x, double y);
__forceinline double MaxF(double x, double y);
template <bool HPopCnt> __forceinline int popcount(uint64 x);
uint64 BMagicAttacks(int i, uint64 occ);
uint64 RMagicAttacks(int i, uint64 occ);
uint16 rand16();
uint64 random();
void init_pst();
void init_eval();
void init();
void init_search(int clear_hash);
void setup_board();
void get_board(const char fen[]);
void init_hash();
void move_to_string(int move, char string[]);
int move_from_string(char string[]);
void pick_pv();
template <bool me> void do_move(int move);
template <bool me> void undo_move(int move);
void do_null();
void undo_null();
__forceinline void evaluate();
template <bool me> int is_legal(int move);
template <bool me> int is_check(int move);
void hash_high(int value, int depth);
void hash_low(int move, int value, int depth);
void hash_exact(int move, int value, int depth, int exclusion, int ex_depth, int knodes);
__forceinline int pick_move();
template <bool me, bool root> int get_move();
template <bool me> int see(int move, int margin);
template <bool me> void gen_root_moves();
template <bool me, bool up> int * gen_captures(int * list);
template <bool me> int * gen_evasions(int * list);
void mark_evasions(int * list);
template <bool me> int * gen_quiet_moves(int * list);
template <bool me> int * gen_checks(int * list);
template <bool me> int * gen_delta_moves(int * list);
template <bool me, bool pv> int q_search(int alpha, int beta, int depth, int flags);
template <bool me, bool pv> int q_evasion(int alpha, int beta, int depth, int flags);
template <bool me, bool exclusion> int search(int beta, int depth, int flags);
template <bool me, bool exclusion> int search_evasion(int beta, int depth, int flags);
template <bool me, bool root> int pv_search(int alpha, int beta, int depth, int flags);
template <bool me> void root();
template <bool me> int multipv(int depth);
void send_pv(int depth, int alpha, int beta, int score);
void send_multipv(int depth, int curr_number);
void send_best_move();
void get_position(char string[]);
void get_time_limit(char string[]);
sint64 get_time();
int time_to_stop(GSearchInfo * SI, int time, int searching);
void check_time(int searching);
void check_time(int time, int searching);
int input();
void uci();




#ifdef WINDOWS_X64
__forceinline int lsb(uint64 x) {
	register unsigned long y;
	_BitScanForward64(&y, x);
	return y;
}

__forceinline int msb(uint64 x) {
	register unsigned long y;
	_BitScanReverse64(&y, x);
	return y;
}

__forceinline int popcnt(uint64 x) {
	x = x - ((x >> 1) & 0x5555555555555555);
	x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
	x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
	return (x * 0x0101010101010101) >> 56;
}

template <bool HPopCnt> __forceinline int popcount(uint64 x) {
	return HPopCnt ? _mm_popcnt_u64(x) : popcnt(x);
	//return HPopCnt ? _mm_countbits_64(x) : popcnt(x);
}
#else
__forceinline int lsb(uint64 x) {
	_asm {
		mov eax, dword ptr x[0]
			test eax, eax
			jz l_high
			bsf eax, eax
			jmp l_ret
		l_high : bsf eax, dword ptr x[4]
				 add eax, 20h
			 l_ret :
	}
}

__forceinline int msb(uint64 x) {
	_asm {
		mov eax, dword ptr x[4]
			test eax, eax
			jz l_low
			bsr eax, eax
			add eax, 20h
			jmp l_ret
		l_low : bsr eax, dword ptr x[0]
			l_ret :
	}
}

__forceinline int popcnt(uint64 x) {
	unsigned int x1, x2;
	x1 = (unsigned int)(x & 0xFFFFFFFF);
	x1 -= (x1 >> 1) & 0x55555555;
	x1 = (x1 & 0x33333333) + ((x1 >> 2) & 0x33333333);
	x1 = (x1 + (x1 >> 4)) & 0x0F0F0F0F;
	x2 = (unsigned int)(x >> 32);
	x2 -= (x2 >> 1) & 0x55555555;
	x2 = (x2 & 0x33333333) + ((x2 >> 2) & 0x33333333);
	x2 = (x2 + (x2 >> 4)) & 0x0F0F0F0F;
	return ((x1 * 0x01010101) >> 24) + ((x2 * 0x01010101) >> 24);
}

template <bool HPopCnt> __forceinline int popcount(uint64 x) {
	return HPopCnt ? (_mm_popcnt_u32((int)x) + _mm_popcnt_u32(x >> 32)) : popcnt(x);
}
#endif

__forceinline int MinF(int x, int y) { return Min(x, y); }
__forceinline int MaxF(int x, int y) { return Max(x, y); }
__forceinline double MinF(double x, double y) { return Min(x, y); }
__forceinline double MaxF(double x, double y) { return Max(x, y); }

uint64 BMagicAttacks(int i, uint64 occ) {
    uint64 att = 0;
    for (uint64 u = BMask[i]; T(u); Cut(u)) if (F(Between[i][lsb(u)] & occ)) att |= Between[i][lsb(u)] | Bit(lsb(u));
	return att;
}

uint64 RMagicAttacks(int i, uint64 occ) {
    uint64 att = 0;
    for (uint64 u = RMask[i]; T(u); Cut(u)) if (F(Between[i][lsb(u)] & occ)) att |= Between[i][lsb(u)] | Bit(lsb(u));
	return att;
}

#ifdef MERSENNE
#include <stdint.h>


#define NN 312
#define MM 156
#define MATRIX_A 0xB5026F5AA96619E9ULL
#define UM 0xFFFFFFFF80000000ULL /* Most significant 33 bits */
#define LM 0x7FFFFFFFULL /* Least significant 31 bits */


/* The array for the state vector */
static uint64_t mt[NN];
/* mti==NN+1 means mt[NN] is not initialized */
static int mti = NN + 1;

/* initializes mt[NN] with a seed */
void init_genrand64(uint64_t seed_)
{
	mt[0] = seed_;
	for (mti = 1; mti < NN; mti++)
		mt[mti] = (6364136223846793005ULL * (mt[mti - 1] ^ (mt[mti - 1] >> 62)) + mti);
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void init_by_array64(uint64_t init_key[],
	uint64_t key_length)
{
	uint64_t i, j, k;
	init_genrand64(19650218ULL);
	i = 1; j = 0;
	k = (NN > key_length ? NN : key_length);
	for (; k; k--) {
		mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 62)) * 3935559000370003845ULL))
			+ init_key[j] + j; /* non linear */
		i++; j++;
		if (i >= NN) { mt[0] = mt[NN - 1]; i = 1; }
		if (j >= key_length) j = 0;
	}
	for (k = NN - 1; k; k--) {
		mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 62)) * 2862933555777941757ULL))
			- i; /* non linear */
		i++;
		if (i >= NN) { mt[0] = mt[NN - 1]; i = 1; }
	}

	mt[0] = 1ULL << 63; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0, 2^64-1]-interval */
uint64 random(void)
{
	int i;
	uint64_t x;
	static uint64_t mag01[2] = { 0ULL, MATRIX_A };

	if (mti >= NN) { /* generate NN words at one time */

		/* if init_genrand64() has not been called, */
		/* a default initial seed is used     */
		if (mti == NN + 1)
			init_genrand64(5489ULL);

		for (i = 0; i < NN - MM; i++) {
			x = (mt[i] & UM) | (mt[i + 1] & LM);
			mt[i] = mt[i + MM] ^ (x >> 1) ^ mag01[(int)(x & 1ULL)];
		}
		for (; i < NN - 1; i++) {
			x = (mt[i] & UM) | (mt[i + 1] & LM);
			mt[i] = mt[i + (MM - NN)] ^ (x >> 1) ^ mag01[(int)(x & 1ULL)];
		}
		x = (mt[NN - 1] & UM) | (mt[0] & LM);
		mt[NN - 1] = mt[MM - 1] ^ (x >> 1) ^ mag01[(int)(x & 1ULL)];

		mti = 0;
	}

	x = mt[mti++];

	x ^= (x >> 29) & 0x5555555555555555ULL;
	x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
	x ^= (x << 37) & 0xFFF7EEE000000000ULL;
	x ^= (x >> 43);

	return x;
}


#else
uint16 rand16() {
	seed = (seed * Convert(6364136223846793005, uint64)) + Convert(1442695040888963407, uint64);
	return Convert((seed >> 32) & 0xFFFF, uint16);
}

uint64 random() {
	uint64 key = Convert(rand16(), uint64); key <<= 16;
	key |= Convert(rand16(), uint64); key <<= 16;
	key |= Convert(rand16(), uint64); key <<= 16;
	return key | Convert(rand16(), uint64);
}
#endif // MERSENNE

void init_misc() {
	int i, j, k, l, n;
	uint64 u;

	for (i = 0; i < 64; i++) {
		HLine[i] = VLine[i] = NDiag[i] = SDiag[i] = RMask[i] = BMask[i] = QMask[i] = 0;
		BMagicMask[i] = RMagicMask[i] = NAtt[i] = SArea[i] = DArea[i] = NArea[i] = 0;
		PAtt[0][i] = PAtt[1][i] = PMove[0][i] = PMove[1][i] = PWay[0][i] = PWay[1][i] = PSupport[0][i] = PSupport[1][i] = BishopForward[0][i] = BishopForward[1][i] = 0;
		for (j = 0; j < 64; j++) Between[i][j] = FullLine[i][j] = 0;
	}

	for (i = 0; i < 64; i++) for (j = 0; j < 64; j++) if (i != j) {
		u = Bit(j);
		if (File(i) == File(j)) VLine[i] |= u;
		if (Rank(i) == Rank(j)) HLine[i] |= u;
		if (NDiag(i) == NDiag(j)) NDiag[i] |= u;
		if (SDiag(i) == SDiag(j)) SDiag[i] |= u;
		if (Dist(i,j) <= 2) {
			DArea[i] |= u;
			if (Dist(i,j) <= 1) SArea[i] |= u;
			if (Abs(Rank(i)-Rank(j)) + Abs(File(i)-File(j)) == 3) NAtt[i] |= u;
		}
		if (j == i + 8) PMove[0][i] |= u;
		if (j == i - 8) PMove[1][i] |= u;
		if (Abs(File(i) - File(j)) == 1) {
			if (Rank(j) >= Rank(i)) {
				PSupport[1][i] |= u;
				if (Rank(j) - Rank(i) == 1) PAtt[0][i] |= u;
			} 
			if (Rank(j) <= Rank(i)) {
				PSupport[0][i] |= u;
				if (Rank(i) - Rank(j) == 1) PAtt[1][i] |= u;
			}
		} else if (File(i) == File(j)) {
			if (Rank(j) > Rank(i)) PWay[0][i] |= u;
			else PWay[1][i] |= u;
		}
	}
	for (i = 0; i < 64; i++) {
		RMask[i] = HLine[i] | VLine[i];
		BMask[i] = NDiag[i] | SDiag[i];
		QMask[i] = RMask[i] | BMask[i];
		BMagicMask[i] = BMask[i] & Interior;
		RMagicMask[i] = RMask[i];
		if (File(i) > 0) RMagicMask[i] &= ~File[0];
		if (Rank(i) > 0) RMagicMask[i] &= ~Line[0];
		if (File(i) < 7) RMagicMask[i] &= ~File[7];
		if (Rank(i) < 7) RMagicMask[i] &= ~Line[7];
		for (j = 0; j < 64; j++) if (NAtt[i] & NAtt[j]) Add(NArea[i],j);
	}
	for (i = 0; i < 8; i++) {
		West[i] = 0;
		East[i] = 0;
		Forward[0][i] = Forward[1][i] = 0;
		PIsolated[i] = 0;
		for (j = 0; j < 8; j++) {
			if (i < j) Forward[0][i] |= Line[j];
			else if (i > j) Forward[1][i] |= Line[j];
			if (i < j) East[i] |= File[j];
			else if (i > j) West[i] |= File[j];
		}
		if (i > 0) PIsolated[i] |= File[i - 1];
		if (i < 7) PIsolated[i] |= File[i + 1];
	}
	for (i = 0; i < 64; i++) {
		for (u = QMask[i]; T(u); Cut(u)) {
			j = lsb(u);
			k = Sgn(Rank(j)-Rank(i));
			l = Sgn(File(j)-File(i));
			for (n = i + 8 * k + l; n != j; n += (8 * k + l)) Add(Between[i][j],n);
		}
		for (u = BMask[i]; T(u); Cut(u)) {
			j = lsb(u);
			FullLine[i][j] = BMask[i] & BMask[j];
		}
		for (u = RMask[i]; T(u); Cut(u)) {
			j = lsb(u);
			FullLine[i][j] = RMask[i] & RMask[j];
		}
		BishopForward[0][i] |= PWay[0][i];
		BishopForward[1][i] |= PWay[1][i];
		for (j = 0; j < 64; j++) {
			if ((PWay[1][j] | Bit(j)) & BMask[i] & Forward[0][Rank(i)]) BishopForward[0][i] |= Bit(j);
			if ((PWay[0][j] | Bit(j)) & BMask[i] & Forward[1][Rank(i)]) BishopForward[1][i] |= Bit(j);
		}
	}

    for (i = 0; i < 16; i++) for (j = 0; j < 16; j++) {
		if (j < WhitePawn) MvvLva[i][j] = 0;
		else if (j < WhiteKnight) MvvLva[i][j] = PawnCaptureMvvLva(i) << 26;
		else if (j < WhiteLight) MvvLva[i][j] = KnightCaptureMvvLva(i) << 26;
		else if (j < WhiteRook) MvvLva[i][j] = BishopCaptureMvvLva(i) << 26;
		else if (j < WhiteQueen) MvvLva[i][j] = RookCaptureMvvLva(i) << 26;
		else MvvLva[i][j] = QueenCaptureMvvLva(i) << 26;
	}

	for (i = 0; i < 256; i++) PieceFromChar[i] = 0;
    PieceFromChar[66] = 6; PieceFromChar[75] = 14; PieceFromChar[78] = 4; PieceFromChar[80] = 2; PieceFromChar[81] = 12; PieceFromChar[82] = 10;
    PieceFromChar[98] = 7; PieceFromChar[107] = 15; PieceFromChar[110] = 5; PieceFromChar[112] = 3; PieceFromChar[113] = 13; PieceFromChar[114] = 11;

	TurnKey = random();
	for (i = 0; i < 8; i++) EPKey[i] = random();
	for (i = 0; i < 16; i++) CastleKey[i] = random();
	for (i = 0; i < 16; i++) for (j = 0; j < 64; j++) {
		if (i == 0) PieceKey[i][j] = 0;
		else PieceKey[i][j] = random();
	}
	for (i = 0; i < 16; i++) LogDist[i] = (int)(10.0 * log(1.01 + (double)i));
}

void init_magic() {
	int i, j, k, index, bit_list[16];
	uint64 u, bits;

	for (i = 0; i < 64; i++) {
		bits = 64 - BShift[i];
		for (u = BMagicMask[i], j = 0; T(u); Cut(u), j++) bit_list[j] = lsb(u);
		for (j = 0; j < Bit(bits); j++) {
			u = 0;
			for (k = 0; k < bits; k++)
				if (Odd(j >> k)) Add(u,bit_list[k]);
#ifndef HNI
			index = Convert(BOffset[i] + ((BMagic[i] * u) >> BShift[i]),int);
#else
			index = Convert(BOffset[i] + _pext_u64(u,BMagicMask[i]),int);
#endif
            MagicAttacks[index] = BMagicAttacks(i,u);
		}
		bits = 64 - RShift[i];
		for (u = RMagicMask[i], j = 0; T(u); Cut(u), j++) bit_list[j] = lsb(u);
		for (j = 0; j < Bit(bits); j++) {
			u = 0;
			for (k = 0; k < bits; k++)
				if (Odd(j >> k)) Add(u,bit_list[k]);
#ifndef HNI
			index = Convert(ROffset[i] + ((RMagic[i] * u) >> RShift[i]),int);
#else
			index = Convert(ROffset[i] + _pext_u64(u,RMagicMask[i]),int);
#endif
             MagicAttacks[index] = RMagicAttacks(i,u);
		}	
	}
}

void gen_kpk() {
	int turn, wp, wk, bk, to, cnt, old_cnt, un;
	uint64 bwp, bwk, bbk, u;
	uint8 Kpk_gen[2][64][64][64];

	memset(Kpk_gen, 0, 2 * 64 * 64 * 64);

	cnt = 0;
	old_cnt = 1;
start:
	if (cnt == old_cnt) goto end;
	old_cnt = cnt;
	cnt = 0;
	for (turn = 0; turn < 2; turn++) {
		for (wp = 0; wp < 64; wp++) {
			for (wk = 0; wk < 64; wk++) {
				for (bk = 0; bk < 64; bk++) {
					if (Kpk_gen[turn][wp][wk][bk]) continue;
					cnt++;
					if (wp < 8 || wp >= 56) goto set_draw;
					if (wp == wk || wk == bk || bk == wp) goto set_draw;
					bwp = Bit(wp);
					bwk = Bit(wk);
					bbk = Bit(bk);
					if (PAtt[White][wp] & bbk) {
						if (turn == White) goto set_draw;
						else if (F(SArea[wk] & bwp)) goto set_draw;
					}
					un = 0;
					if (turn == Black) {
						u = SArea[bk] & (~(SArea[wk] | PAtt[White][wp]));
						if (F(u)) goto set_draw;
						for (; T(u); Cut(u)) {
							to = lsb(u);
							if (Kpk_gen[turn ^ 1][wp][wk][to] == 1) goto set_draw;
							else if (Kpk_gen[turn ^ 1][wp][wk][to] == 0) un++;
						}
						if (F(un)) goto set_win;
					}
					else {
						for (u = SArea[wk] & (~(SArea[bk] | bwp)); T(u); Cut(u)) {
							to = lsb(u);
							if (Kpk_gen[turn ^ 1][wp][to][bk] == 2) goto set_win;
							else if (Kpk_gen[turn ^ 1][wp][to][bk] == 0) un++;
						}
						to = wp + 8;
						if (to != wk && to != bk) {
							if (to >= 56) {
								if (F(SArea[to] & bbk)) goto set_win;
								if (SArea[to] & bwk) goto set_win;
							}
							else {
								if (Kpk_gen[turn ^ 1][to][wk][bk] == 2) goto set_win;
								else if (Kpk_gen[turn ^ 1][to][wk][bk] == 0) un++;
								if (to < 24) {
									to += 8;
									if (to != wk && to != bk) {
										if (Kpk_gen[turn ^ 1][to][wk][bk] == 2) goto set_win;
										else if (Kpk_gen[turn ^ 1][to][wk][bk] == 0) un++;
									}
								}
							}
						}
						if (F(un)) goto set_draw;
					}
					continue;
				set_draw:
					Kpk_gen[turn][wp][wk][bk] = 1;
					continue;
				set_win:
					Kpk_gen[turn][wp][wk][bk] = 2;
					continue;
				}
			}
		}
	}
	if (cnt) goto start;
end:
	for (turn = 0; turn < 2; turn++) {
		for (wp = 0; wp < 64; wp++) {
			for (wk = 0; wk < 64; wk++) {
				Kpk[turn][wp][wk] = 0;
				for (bk = 0; bk < 64; bk++) {
					if (Kpk_gen[turn][wp][wk][bk] == 2) Kpk[turn][wp][wk] |= Bit(bk);
				}
			}
		}
	}
}

void init_pst() {
	int i, j, k, op, eg, index, r, f, d, e, distQ[4], distL[4], distM[2];
	memset(Pst,0,16 * 64 * sizeof(int));

	for (i = 0; i < 64; i++) {
		r = Rank(i);
		f = File(i);
		d = Abs(f - r);
		e = Abs(f + r - 7);
		distQ[0] = DistC[f] * DistC[f]; distL[0] = DistC[f];
		distQ[1] = DistC[r] * DistC[r]; distL[1] = DistC[r];
		distQ[2] = RankR[d] * RankR[d] + RankR[e] * RankR[e]; distL[2] = RankR[d] + RankR[e];
		distQ[3] = RankR[r] * RankR[r]; distL[3] = RankR[r];
		distM[0] = DistC[f] * DistC[r]; distM[1] = DistC[f] * RankR[r];
		for (j = 2; j < 16; j += 2) {
			index = PieceType[j];
			op = eg = 0;
			for (k = 0; k < 2; k++) {
				op += Av(PstQuadMixedWeights, 4, index, (k * 2)) * distM[k];
				eg += Av(PstQuadMixedWeights, 4, index, (k * 2) + 1) * distM[k];
			}
			for (k = 0; k < 4; k++) {
				op += Av(PstQuadWeights,8,index,(k * 2)) * distQ[k];
				eg += Av(PstQuadWeights,8,index,(k * 2) + 1) * distQ[k];
				op += Av(PstLinearWeights,8,index,(k * 2)) * distL[k];
				eg += Av(PstLinearWeights,8,index,(k * 2) + 1) * distL[k];
			}
			Pst(j,i) = Compose(op/64, eg/64);
		}
	}

	Pst(WhiteKnight,56) -= Compose(100, 0);
	Pst(WhiteKnight,63) -= Compose(100, 0);
	for (i = 0; i < 64; i++) {
		for (j = 3; j < 16; j+=2) {
			op = Opening(Pst(j-1,63-i));
			eg = Endgame(Pst(j-1,63-i));
			Pst(j,i) = Compose(-op,-eg);
		}
	}
	Current->pst = 0;
	for (i = 0; i < 64; i++)
	if (Square(i)) Current->pst += Pst(Square(i),i);
}

void init_eval() {
	int i, j, k, index;
	memset(Mobility,0,4 * 32 * sizeof(int));
	for (i = 0; i < 4; i++) for (j = 0; j < 32; j++) {
		index = i * 2;
		double op = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		index = i * 2 + 1;
		double eg = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		Mobility[i][j] = Compose((int)(op/64.0),(int)(eg/64.0));
	}
	
	for (i = 0; i < 3; i++) for (j = 7; j >= 0; j--) {
		Shelter[i][j] = 0;
		if (j > 1) for (k = 1; k < Min(j, 5); k++) Shelter[i][j] += Av(ShelterValue, 0, 0, (i * 5) + k - 1);
		if (!j) Shelter[i][j] = Shelter[i][7] + Av(ShelterValue, 0, 0, (i * 5) + 4);
	}

	for (i = 0; i < 4; i++) {
		StormBlocked[i] = ((Sa(StormQuad, StormBlockedMul) * i * i) + (Sa(StormLinear, StormBlockedMul) * (i + 1))) / 100;
		StormShelterAtt[i] = ((Sa(StormQuad, StormShelterAttMul) * i * i) + (Sa(StormLinear, StormShelterAttMul) * (i + 1))) / 100;
		StormConnected[i] = ((Sa(StormQuad, StormConnectedMul) * i * i) + (Sa(StormLinear, StormConnectedMul) * (i + 1))) / 100;
		StormOpen[i] = ((Sa(StormQuad, StormOpenMul) * i * i) + (Sa(StormLinear, StormOpenMul) * (i + 1))) / 100;
		StormFree[i] = ((Sa(StormQuad, StormFreeMul) * i * i) + (Sa(StormLinear, StormFreeMul) * (i + 1))) / 100;
	}

	for (i = 0; i < 8; i++) {
		int l = Max(i - 2, 0);
		int q = l * l;
		PasserGeneral[i] = Compose16(Av(PasserQuad, 2, 0, 0) * q + Av(PasserLinear, 2, 0, 0) * l, Av(PasserQuad, 2, 0, 1) * q + Av(PasserLinear, 2, 0, 1) * l);
		PasserBlocked[i] = Compose16(Av(PasserQuad, 2, 1, 0) * q + Av(PasserLinear, 2, 1, 0) * l, Av(PasserQuad, 2, 1, 1) * q + Av(PasserLinear, 2, 1, 1) * l);
		PasserFree[i] = Compose16(Av(PasserQuad, 2, 2, 0) * q + Av(PasserLinear, 2, 2, 0) * l, Av(PasserQuad, 2, 2, 1) * q + Av(PasserLinear, 2, 2, 1) * l);
		PasserSupported[i] = Compose16(Av(PasserQuad, 2, 3, 0) * q + Av(PasserLinear, 2, 3, 0) * l, Av(PasserQuad, 2, 3, 1) * q + Av(PasserLinear, 2, 3, 1) * l);
		PasserProtected[i] = Compose16(Av(PasserQuad, 2, 4, 0) * q + Av(PasserLinear, 2, 4, 0) * l, Av(PasserQuad, 2, 4, 1) * q + Av(PasserLinear, 2, 4, 1) * l);
		PasserConnected[i] = Compose16(Av(PasserQuad, 2, 5, 0) * q + Av(PasserLinear, 2, 5, 0) * l, Av(PasserQuad, 2, 5, 1) * q + Av(PasserLinear, 2, 5, 1) * l);
		PasserOutside[i] = Compose16(Av(PasserQuad, 2, 6, 0) * q + Av(PasserLinear, 2, 6, 0) * l, Av(PasserQuad, 2, 6, 1) * q + Av(PasserLinear, 2, 6, 1) * l);
		PasserCandidate[i] = Compose16(Av(PasserQuad, 2, 7, 0) * q + Av(PasserLinear, 2, 7, 0) * l, Av(PasserQuad, 2, 7, 1) * q + Av(PasserLinear, 2, 7, 1) * l);
		PasserClear[i] = Compose16(Av(PasserQuad, 2, 8, 0) * q + Av(PasserLinear, 2, 8, 0) * l, Av(PasserQuad, 2, 8, 1) * q + Av(PasserLinear, 2, 8, 1) * l);

		PasserAtt[i] = Av(PasserAttDefQuad, 2, 0, 0) * q + Av(PasserAttDefLinear, 2, 0, 0) * l;
		PasserDef[i] = Av(PasserAttDefQuad, 2, 1, 0) * q + Av(PasserAttDefLinear, 2, 1, 0) * l;
		PasserAttLog[i] = Av(PasserAttDefQuad, 2, 0, 1) * q + Av(PasserAttDefLinear, 2, 0, 1) * l;
		PasserDefLog[i] = Av(PasserAttDefQuad, 2, 1, 1) * q + Av(PasserAttDefLinear, 2, 1, 1) * l;
	}
}

void calc_material(int index) {
	int pawns[2], knights[2], light[2], dark[2], rooks[2], queens[2], bishops[2], major[2], minor[2], tot[2], mat[2], mul[2], quad[2], score, phase, me, i = index;

	queens[White] = i % 3; i /= 3;
	queens[Black] = i % 3; i /= 3;
	rooks[White] = i % 3; i /= 3;
	rooks[Black] = i % 3; i /= 3;
	light[White] = i % 2; i /= 2;
	light[Black] = i % 2; i /= 2;
	dark[White] = i % 2; i /= 2;
	dark[Black] = i % 2; i /= 2;
	knights[White] = i % 3; i /= 3;
	knights[Black] = i % 3; i /= 3;
	pawns[White] = i % 9; i /= 9;
	pawns[Black] = i % 9;
	for (me = 0; me < 2; me++) {
		bishops[me] = light[me] + dark[me];
		major[me] = rooks[me] + queens[me];
		minor[me] = bishops[me] + knights[me];
		tot[me] = 3 * minor[me] + 5 * rooks[me] + 9 * queens[me];
		mat[me] = mul[me] = 32;
		quad[me] = 0;
	}
	score = (SeeValue[WhitePawn] + Av(MatLinear, 0, 0, 0)) * (pawns[White] - pawns[Black]) + (SeeValue[WhiteKnight] + Av(MatLinear, 0, 0, 1)) * (knights[White] - knights[Black])
		+ (SeeValue[WhiteLight] + Av(MatLinear, 0, 0, 2)) * (bishops[White] - bishops[Black]) + (SeeValue[WhiteRook] + Av(MatLinear, 0, 0, 3)) * (rooks[White] - rooks[Black])
		+ (SeeValue[WhiteQueen] + Av(MatLinear, 0, 0, 4)) * (queens[White] - queens[Black]) + 50 * ((bishops[White] / 2) - (bishops[Black] / 2));
	phase = Phase[PieceType[WhitePawn]] * (pawns[White] + pawns[Black]) + Phase[PieceType[WhiteKnight]] * (knights[White] + knights[Black])
		+ Phase[PieceType[WhiteLight]] * (bishops[White] + bishops[Black]) + Phase[PieceType[WhiteRook]] * (rooks[White] + rooks[Black])
		+ Phase[PieceType[WhiteQueen]] * (queens[White] + queens[Black]);
	Material[index].phase = Min((Max(phase - PhaseMin, 0) * 128) / (PhaseMax - PhaseMin), 128);

	int special = 0;
	for (me = 0; me < 2; me++) {
		if (queens[me] == queens[opp]) {
			if (rooks[me] - rooks[opp] == 1) {
				if (knights[me] == knights[opp] && bishops[opp] - bishops[me] == 1) IncV(special, Ca(MatSpecial, MatRB));
				else if (bishops[me] == bishops[opp] && knights[opp] - knights[me] == 1) IncV(special, Ca(MatSpecial, MatRN));
				else if (knights[me] == knights[opp] && bishops[opp] - bishops[me] == 2) DecV(special, Ca(MatSpecial, MatBBR));
				else if (bishops[me] == bishops[opp] && knights[opp] - knights[me] == 2) DecV(special, Ca(MatSpecial, MatNNR));
				else if (bishops[opp] - bishops[me] == 1 && knights[opp] - knights[me] == 1) DecV(special, Ca(MatSpecial, MatBNR));
			} else if (rooks[me] == rooks[opp] && minor[me] - minor[opp] == 1) IncV(special, Ca(MatSpecial, MatM));
		} else if (queens[me] - queens[opp] == 1) {
			if (rooks[opp] - rooks[me] == 2 && minor[opp] - minor[me] == 0) IncV(special, Ca(MatSpecial, MatQRR));
			else if (rooks[opp] - rooks[me] == 1 && knights[opp] == knights[me] && bishops[opp] - bishops[me] == 1) IncV(special, Ca(MatSpecial, MatQRB));
			else if (rooks[opp] - rooks[me] == 1 && knights[opp] - knights[me] == 1 && bishops[opp] == bishops[me]) IncV(special, Ca(MatSpecial, MatQRN));
			else if ((major[opp] + minor[opp]) - (major[me] + minor[me]) >= 2) IncV(special, Ca(MatSpecial, MatQ3));
		}
	}
	score += (Opening(special) * Material[index].phase + Endgame(special) * (128 - (int)Material[index].phase)) / 128;

	for (me = 0; me < 2; me++) {
		quad[me] += pawns[me] * (pawns[me] * TrAv(MatQuadMe, 5, 0, 0) + knights[me] * TrAv(MatQuadMe, 5, 0, 1)
			+ bishops[me] * TrAv(MatQuadMe, 5, 0, 2) + rooks[me] * TrAv(MatQuadMe, 5, 0, 3) + queens[me] * TrAv(MatQuadMe, 5, 0, 4));
		quad[me] += knights[me] * (knights[me] * TrAv(MatQuadMe, 5, 1, 0)
			+ bishops[me] * TrAv(MatQuadMe, 5, 1, 1) + rooks[me] * TrAv(MatQuadMe, 5, 1, 2) + queens[me] * TrAv(MatQuadMe, 5, 1, 3));
		quad[me] += bishops[me] * (bishops[me] * TrAv(MatQuadMe, 5, 2, 0) + rooks[me] * TrAv(MatQuadMe, 5, 2, 1) + queens[me] * TrAv(MatQuadMe, 5, 2, 2));

		quad[me] += rooks[me] * (rooks[me] * TrAv(MatQuadMe, 5, 3, 0) + queens[me] * TrAv(MatQuadMe, 5, 3, 1));
		quad[me] += pawns[me] * (knights[opp] * TrAv(MatQuadOpp, 4, 0, 0)
			+ bishops[opp] * TrAv(MatQuadOpp, 4, 0, 1) + rooks[opp] * TrAv(MatQuadOpp, 4, 0, 2) + queens[opp] * TrAv(MatQuadOpp, 4, 0, 3));
		quad[me] += knights[me] * (bishops[opp] * TrAv(MatQuadOpp, 4, 1, 0) + rooks[opp] * TrAv(MatQuadOpp, 4, 1, 1) + queens[opp] * TrAv(MatQuadOpp, 4, 1, 2));
		quad[me] += bishops[me] * (rooks[opp] * TrAv(MatQuadOpp, 4, 2, 0) + queens[opp] * TrAv(MatQuadOpp, 4, 2, 1));
		quad[me] += rooks[me] * queens[opp] * TrAv(MatQuadOpp, 4, 3, 0);

		if (bishops[me] >= 2) quad[me] += pawns[me] * Av(BishopPairQuad, 0, 0, 0) + knights[me] * Av(BishopPairQuad, 0, 0, 1) + rooks[me] * Av(BishopPairQuad, 0, 0, 2)
			+ queens[me] * Av(BishopPairQuad, 0, 0, 3) + pawns[opp] * Av(BishopPairQuad, 0, 0, 4) + knights[opp] * Av(BishopPairQuad, 0, 0, 5)
			+ bishops[opp] * Av(BishopPairQuad, 0, 0, 6) + rooks[opp] * Av(BishopPairQuad, 0, 0, 7) + queens[opp] * Av(BishopPairQuad, 0, 0, 8);
	}
	score += (quad[White] - quad[Black]) / 100;

	for (me = 0; me < 2; me++) {
		if (tot[me] - tot[opp] <= 3) {
			if (!pawns[me]) {
				if (tot[me] <= 3) mul[me] = 0;
				if (tot[me] == tot[opp] && major[me] == major[opp] && minor[me] == minor[opp]) mul[me] = major[me] + minor[me] <= 2 ? 0 : (major[me] + minor[me] <= 3 ? 16 : 32);
				else if (minor[me] + major[me] <= 2) {
					if (bishops[me] < 2) mat[me] = (bishops[me] && rooks[me]) ? 8 : 1;
					else if (bishops[opp] + rooks[opp] >= 1) mat[me] = 1;
					else mat[me] = 32;
				} else if (tot[me] - tot[opp] < 3 && minor[me] + major[me] - minor[opp] - major[opp] <= 1) mat[me] = 4;
				else if (minor[me] + major[me] <= 3) mat[me] = 8 * (1 + bishops[me]);
				else mat[me] = 8 * (2 + bishops[me]);
			}
			if (pawns[me] <= 1) {
				mul[me] = Min(28, mul[me]);
				if (rooks[me] == 1 && queens[me] + minor[me] == 0 && rooks[opp] == 1) mat[me] = Min(23, mat[me]);
			}
		}
		if (!major[me]) {
			if (!minor[me]) {
				if (!tot[me] && pawns[me] < pawns[opp]) DecV(score, (pawns[opp] - pawns[me]) * SeeValue[WhitePawn]);
			} else if (minor[me] == 1) {
				if (pawns[me] <= 1 && minor[opp] >= 1) mat[me] = 1;
				if (bishops[me] == 1) {
					if (minor[opp] == 1 && bishops[opp] == 1 && light[me] != light[opp]) {
						mul[me] = Min(mul[me], 15);
						if (pawns[me] - pawns[opp] <= 1) mul[me] = Min(mul[me], 11);
					}
				}
			} else if (!pawns[me] && knights[me] == 2 && !bishops[me]) {
				if (!tot[opp] && pawns[opp]) mat[me] = 6;
				else mul[me] = 0;
			}
		}
		if (!mul[me]) mat[me] = 0;
		if (mat[me] <= 1 && tot[me] != tot[opp]) mul[me] = Min(mul[me], 8);
	}
	if (bishops[White] == 1 && bishops[Black] == 1 && light[White] != light[Black]) {
		mul[White] = Min(mul[White], 24 + 2 * (knights[Black] + major[Black]));
		mul[Black] = Min(mul[Black], 24 + 2 * (knights[White] + major[White]));
	} else if (!minor[White] && !minor[Black] && major[White] == 1 && major[Black] == 1 && rooks[White] == rooks[Black]) {
		mul[White] = Min(mul[White], 25);
		mul[Black] = Min(mul[Black], 25);
	}
	for (me = 0; me < 2; me++) {
		Material[index].mul[me] = mul[me];
		Material[index].pieces[me] = major[me] + minor[me];
	}
	if (score > 0) score = (score * mat[White]) / 32;
	else score = (score * mat[Black]) / 32;
	Material[index].score = score;
	for (me = 0; me < 2; me++) {
		if (major[me] == 0 && minor[me] == bishops[me] && minor[me] <= 1) Material[index].flags |= VarC(FlagSingleBishop, me);
		if (((major[me] == 0 || minor[me] == 0) && major[me] + minor[me] <= 1) || major[opp] + minor[opp] == 0
			|| (!pawns[me] && major[me] == rooks[me] && major[me] == 1 && minor[me] == bishops[me] && minor[me] == 1 && rooks[opp] == 1 && !minor[opp] && !queens[opp])) Material[index].flags |= VarC(FlagCallEvalEndgame, me);
	}
}

void init_material() {

	memset(Material,0,TotalMat * sizeof(GMaterial));
	for (int index = 0; index < TotalMat; index++) calc_material(index);
}

void init_hash()		
{

	char name[256];
	sint64 size = (hash_size * sizeof(GEntry));
	sprintf_s(name, "GULL_HASH_%d", WinParId);
	int initialized = 0;
	if (parent && HASH != NULL) 
	{
		initialized = 1;
		UnmapViewOfFile(Hash);
		CloseHandle(HASH);
	}
	if (parent)
	
	{
		if (!LargePages)
			
			goto no_lp;
#ifndef LARGE_PAGES
		goto no_lp;
#endif
		typedef int(*GETLARGEPAGEMINIMUM)(void);
		GETLARGEPAGEMINIMUM pGetLargePageMinimum;
		HINSTANCE hDll = LoadLibrary(TEXT("kernel32.dll"));
		if (hDll == NULL) goto no_lp;
		pGetLargePageMinimum = (GETLARGEPAGEMINIMUM)GetProcAddress(hDll, "GetLargePageMinimum");
		if (pGetLargePageMinimum == NULL) goto no_lp;
		int min_page_size = (*pGetLargePageMinimum)();
		if (size < min_page_size) size = min_page_size;
		if (!initialized) 
		{
			TOKEN_PRIVILEGES tp;
			HANDLE hToken;
			OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken);
			LookupPrivilegeValue(NULL, "SeLockMemoryPrivilege", &tp.Privileges[0].Luid);
			tp.PrivilegeCount = 1;
			tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
			AdjustTokenPrivileges(hToken, FALSE, &tp, 0, (PTOKEN_PRIVILEGES)NULL, 0);
		}
		HASH = NULL;
		HASH = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE | SEC_COMMIT | SEC_LARGE_PAGES, size >> 32, size & 0xFFFFFFFF, name);
		if (HASH != NULL) 
		{
			fprintf(stdout, "Large page hash\n");
			goto hash_allocated;
		}
	no_lp:
		HASH = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, size >> 32, size & 0xFFFFFFFF, name);
	}
	else HASH = OpenFileMapping(FILE_MAP_ALL_ACCESS, 0, name);
hash_allocated:
	Hash = (GEntry*)MapViewOfFile(HASH, FILE_MAP_ALL_ACCESS, 0, 0, size);
	if (parent) memset(Hash, 0, size);
	hash_mask = hash_size - 4;
}


void init_shared() {

	char name[256];
	sint64 size = SharedPVHashOffset + pv_hash_size * sizeof(GPVEntry);
	sprintf_s(name, "GULL_SHARED_%d", WinParId);
	if (parent && SHARED != NULL) {
		UnmapViewOfFile(Smpi);
		CloseHandle(SHARED);
	}
	if (parent) SHARED = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, size, name);
	else SHARED = OpenFileMapping(FILE_MAP_ALL_ACCESS, 0, name);
	Smpi = (GSMPI*)MapViewOfFile(SHARED, FILE_MAP_ALL_ACCESS, 0, 0, size);
	if (parent) memset(Smpi, 0, size);
	Material = (GMaterial*)(((char*)Smpi) + SharedMaterialOffset);
	MagicAttacks = (uint64*)(((char*)Smpi) + SharedMagicOffset);
	PVHash = (GPVEntry*)(((char*)Smpi) + SharedPVHashOffset);
	if (parent) memset(PVHash, 0, pv_hash_size * sizeof(GPVEntry));
}

void init() {
	init_shared();
	init_misc();
	if (parent) init_magic();
	for (int i = 0; i < 64; i++) {
		BOffsetPointer[i] = MagicAttacks + BOffset[i];
		ROffsetPointer[i] = MagicAttacks + ROffset[i];
	}
	gen_kpk();
	init_pst();
	init_eval();
	if (parent) init_material();

}

void init_search(int clear_hash) {
	memset(History,1,16 * 64 * sizeof(sint16));
	memset(Delta,0,16 * 4096 * sizeof(sint16));
	memset(Ref,0,16 * 64 * sizeof(GRef));
	memset(Data + 1, 0, 127 * sizeof(GData));
	if (clear_hash) {
		date = 0;
		date = 1;
		memset(Hash,0,hash_size * sizeof(GEntry));
		memset(PVHash,0,pv_hash_size * sizeof(GPVEntry));
	}
	get_board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
	nodes = 0;
	best_move = best_score = 0;
	LastTime = LastValue = LastExactValue = InstCnt = 0;
	LastSpeed = 0;
	PVN = 1;
	
	Infinite = 1;
	SearchMoves = 0;
	TimeLimit1 = TimeLimit2 = 0;
	Stop = Searching = 0;
	if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
	DepthLimit = 256;
	LastDepth = 256;
	Print = 1;
	memset(CurrentSI,0,sizeof(GSearchInfo));
	memset(BaseSI,0,sizeof(GSearchInfo));
#ifdef CPU_TIMING
	GlobalTime[GlobalTurn] = UciBaseTime;
	GlobalInc[GlobalTurn] = UciIncTime;
#endif
}

void setup_board() {
	int i;
	uint64 occ;
	GEntry * Entry;
	GPVEntry * PVEntry;

	occ = 0;
	sp = 0;
	date++;
	if (date > 0x8000) { // musn't ever happen
		date = 2;
		// now GUI must wait for readyok... we have plenty of time :)
		for (Entry = Hash, i = 0; i < hash_size; i++, Entry++) Entry->date = 1;
		for (PVEntry = PVHash, i = 0; i < pv_hash_size; i++, PVEntry++) PVEntry->date = 1;
	}
	Current->material = 0;
	Current->pst = 0;
	Current->key = PieceKey[0][0];
	if (Current->turn) Current->key ^= TurnKey;
	Current->key ^= CastleKey[Current->castle_flags];
	if (Current->ep_square) Current->key ^= EPKey[File(Current->ep_square)];
	Current->pawn_key = 0;
	Current->pawn_key ^= CastleKey[Current->castle_flags];
	for (i = 0; i < 16; i++) BB(i) = 0;
	for (i = 0; i < 64; i++) {
		if (Square(i)) {
		    Add(BB(Square(i)),i);
		    Add(BB(Square(i) & 1),i);
		    Add(occ,i);
		    Current->key ^= PieceKey[Square(i)][i];
		    if (Square(i) < WhiteKnight) Current->pawn_key ^= PieceKey[Square(i)][i];
			if (Square(i) < WhiteKing) Current->material += MatCode[Square(i)];
			else Current->pawn_key ^= PieceKey[Square(i)][i];
			Current->pst += Pst(Square(i),i);
		}
	}
	if (popcnt(BB(WhiteKnight)) > 2 || popcnt(BB(WhiteLight)) > 1 || popcnt(BB(WhiteDark)) > 1 
		|| popcnt(BB(WhiteRook)) > 2 || popcnt(BB(WhiteQueen)) > 2) Current->material |= FlagUnusualMaterial; 
	if (popcnt(BB(BlackKnight)) > 2 || popcnt(BB(BlackLight)) > 1 || popcnt(BB(BlackDark)) > 1 
		|| popcnt(BB(BlackRook)) > 2 || popcnt(BB(BlackQueen)) > 2) Current->material |= FlagUnusualMaterial; 
	Current->capture = 0;
	Current->killer[1] = Current->killer[2] = 0;
	Current->ply = 0;
	Current->piece_nb = popcnt(PieceAll); // TB
	Stack[sp] = Current->key;
}

void get_board(const char fen[]) {
	int pos, i, j;
	unsigned char c;

	Current = Data;
	memset(Board,0,sizeof(GBoard));
	memset(Current,0,sizeof(GData));
	pos = 0;
	c = fen[pos];
	while (c == ' ') c = fen[++pos];
	for (i = 56; i >= 0; i -= 8) {
		for (j = 0; j <= 7; ) {
            if (c <= '8') j += c - '0';
			else {
				Square(i+j) = PieceFromChar[c];
				if (Even(SDiag(i+j)) && (Square(i+j)/2) == 3) Square(i+j) += 2;
				j++;
			}
			c = fen[++pos];
		}
		c = fen[++pos];
	}
	if (c == 'b') Current->turn = 1;
	c = fen[++pos]; c = fen[++pos];
    if (c == '-') c = fen[++pos];
	if (c == 'K') { Current->castle_flags |= CanCastle_OO; c = fen[++pos]; }
	if (c == 'Q') { Current->castle_flags |= CanCastle_OOO; c = fen[++pos]; }
	if (c == 'k') { Current->castle_flags |= CanCastle_oo; c = fen[++pos]; }
	if (c == 'q') { Current->castle_flags |= CanCastle_ooo; c = fen[++pos]; }
	c = fen[++pos];
	if (c != '-') {
        i = c + fen[++pos] * 8 - 489;
		j = i ^ 8;
		if (Square(i) != 0) i = 0;
		else if (Square(j) != (3 - Current->turn)) i = 0;
		else if (Square(j-1) != (Current->turn+2) && Square(j+1) != (Current->turn+2)) i = 0;
		Current->ep_square = i;
	}
	setup_board();
}

__forceinline GEntry * probe_hash() {
	for (GEntry * Entry = Hash + (High32(Current->key) & hash_mask); Entry < (Hash + (High32(Current->key) & hash_mask)) + 4; Entry++) if (Low32(Current->key) == Entry->key) {
		Entry->date = date;
		return Entry;
	}
	return NULL;
}

__forceinline GPVEntry * probe_pv_hash() {
	for (GPVEntry * PVEntry = PVHash + (High32(Current->key) & pv_hash_mask); PVEntry < PVHash + (High32(Current->key) & pv_hash_mask) + pv_cluster_size; PVEntry++) if (Low32(Current->key) == PVEntry->key) {
		PVEntry->date = date;
		return PVEntry;
	}
	return NULL;
}

void move_to_string(int move, char string[]) { 
	int pos = 0;
    string[pos++] = ((move >> 6) & 7) + 'a';
    string[pos++] = ((move >> 9) & 7) + '1';
    string[pos++] = (move & 7) + 'a';
    string[pos++] = ((move >> 3) & 7) + '1';
    if (IsPromotion(move)) {
        if ((move & 0xF000) == FlagPQueen)  string[pos++] = 'q';
        else if ((move & 0xF000) == FlagPRook)   string[pos++] = 'r';
        else if ((move & 0xF000) == FlagPLight || (move & 0xF000) == FlagPDark) string[pos++] = 'b';
        else if ((move & 0xF000) == FlagPKnight) string[pos++] = 'n';
    }
    string[pos] = 0;
}

int move_from_string(char string[]) { 
	int from, to, move;
    from = ((string[1] - '1') * 8) + (string[0] - 'a');
    to  = ((string[3] - '1') * 8) + (string[2] - 'a');
    move = (from << 6) | to;
    if (Board->square[from] >= WhiteKing && Abs(to - from) == 2) move |= FlagCastling;
    if (T(Current->ep_square) && to == Current->ep_square) move |= FlagEP;
    if (string[4] != 0) {
        if (string[4] == 'q') move |= FlagPQueen;
        else if (string[4] == 'r') move |= FlagPRook;
        else if (string[4] == 'b') {
			if (Odd(to ^ Rank(to))) move |= FlagPLight;
			else move |= FlagPDark;
		} else if (string[4] == 'n') move |= FlagPKnight;
    }
    return move;
}

void pick_pv() {
	GEntry * Entry;
	GPVEntry * PVEntry;
	int i, depth, move;
	if (pvp >= Min(pv_length,64)) {
		PV[pvp] = 0;
		return;
	}
	move = 0;
	depth = -256;
	if (Entry = probe_hash()) if (T(Entry->move) && Entry->low_depth > depth) {
		depth = Entry->low_depth;
		move = Entry->move;
	}
	if (PVEntry = probe_pv_hash()) if (T(PVEntry->move) && PVEntry->depth > depth) {
		depth = PVEntry->depth;
		move = PVEntry->move;
	}
	evaluate();
	if (Current->att[Current->turn] & King(Current->turn ^ 1)) PV[pvp] = 0;
	else if (move && (Current->turn ? is_legal<1>(move) : is_legal<0>(move))) {
		PV[pvp] = move;
		pvp++;
		if (Current->turn) do_move<1>(move);
		else do_move<0>(move);
		if (Current->ply >= 100) goto finish;
		for (i = 4; i <= Current->ply; i+= 2) if (Stack[sp-i] == Current->key) {
			PV[pvp] = 0;
			goto finish;
		}
		pick_pv();
finish:
		if (Current->turn ^ 1) undo_move<1>(move);
		else undo_move<0>(move);
	} else PV[pvp] = 0;
}

template <bool me> int draw_in_pv() {
	if ((Current - Data) >= 126) return 1;
	if (Current->ply >= 100) return 1;
	for (int i = 4; i <= Current->ply; i += 2) if (Stack[sp - i] == Current->key) return 1;
	if (GPVEntry * PVEntry = probe_pv_hash()) {
		if (!PVEntry->value) return 1;
		if (int move = PVEntry->move) {
			do_move<me>(move);
			int value = draw_in_pv<opp>();
			undo_move<me>(move);
			return value;
		}
	}
	return 0;
}

template <bool me> void do_move(int move) {
	GEntry * Entry;
	GPawnEntry * PawnEntry;
	int from, to, piece, capture;
	GData * Next;
	uint64 u, mask_from, mask_to;

	to = To(move);
	Next = Current + 1;
	Next->ep_square = 0;
	capture = Square(to);
    if (F(capture)) {
		Next->capture = 0;
		goto non_capture;
    }
	from = From(move);
	piece = Square(from);
	Next->turn = opp;
	Next->capture = capture;
	Next->piece_nb = Current->piece_nb - 1; // TB
	Square(from) = 0;
	Square(to) = piece;
	Next->piece = piece;
	mask_from = Bit(from);
	mask_to = Bit(to);
	BB(piece) ^= mask_from;
	Piece(me) ^= mask_from;
	BB(capture) ^= mask_to;
	Piece(opp) ^= mask_to;
	BB(piece) |= mask_to;
	Piece(me) |= mask_to;
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from) - Pst(capture,to);
	Next->key = Current->key ^ PieceKey[piece][from] ^ PieceKey[piece][to] ^ PieceKey[capture][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	if (capture != IPawn(opp)) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags]; // of course we can put a lot of operations inside this "if {}" but the speedup won't be worth the effort
	else Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(opp)][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material - MatCode[capture];
	if (T(Current->material & FlagUnusualMaterial) && capture >= WhiteKnight) {
		if (popcnt(BB(WhiteQueen)) <= 2 && popcnt(BB(BlackQueen)) <= 2) {
			if (popcnt(BB(WhiteLight)) <= 1 && popcnt(BB(BlackLight)) <= 1 && popcnt(BB(WhiteKnight)) <= 2
				&& popcnt(BB(BlackKnight)) <= 2 && popcnt(BB(WhiteRook)) <= 2 && popcnt(BB(BlackRook)) <= 2)
				Next->material ^= FlagUnusualMaterial;
		}
	}
	if (piece == IPawn(me)) {
		Next->pawn_key ^= PieceKey[IPawn(me)][from] ^ PieceKey[piece][to];
		if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		}
		PawnEntry = PawnHash + (Next->pawn_key & pawn_hash_mask);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (piece >= WhiteKing) {
		Next->pawn_key ^= PieceKey[piece][from] ^ PieceKey[piece][to];
		PawnEntry = PawnHash + (Next->pawn_key & pawn_hash_mask);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (capture < WhiteKnight) {
		PawnEntry = PawnHash + (Next->pawn_key & pawn_hash_mask);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	}
	if (F(Next->material & FlagUnusualMaterial)) prefetch((char *)(Material + Next->material), _MM_HINT_NTA); 
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = Current->turn ^ 1;
	Next->key ^= TurnKey;
	Entry = Hash + (High32(Next->key) & hash_mask);
	prefetch((char *)Entry,_MM_HINT_NTA);
	Next->ply = 0;
	goto finish;
non_capture:
	from = From(move);
	Next->ply = Current->ply + 1;
	piece = Square(from);
	Square(from) = 0;
	Square(to) = piece;
	Next->piece = piece;
	mask_from = Bit(from);
	mask_to = Bit(to);
	BB(piece) ^= mask_from;
	Piece(me) ^= mask_from;
	BB(piece) |= mask_to;
	Piece(me) |= mask_to;
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->piece_nb = Current->piece_nb; // TB
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from);
	Next->key = Current->key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material;
	if (piece == IPawn(me)) {
		Next->ply = 0;
		Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(me)][to] ^ PieceKey[IPawn(me)][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		if (IsEP(move)) {
			Square(to ^ 8) = 0;
			u = Bit(to ^ 8);
			Next->key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pawn_key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pst -= Pst(IPawn(opp),to ^ 8);
			Pawn(opp) &= ~u;
			Piece(opp) &= ~u;
			Next->material -= MatCode[IPawn(opp)];
			Next->piece_nb--; // TB
		} else if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		} else if ((to ^ from) == 16) {
			if (PAtt[me][(to + from) >> 1] & Pawn(opp)) {
				Next->ep_square = (to + from) >> 1;
				Next->key ^= EPKey[File(Next->ep_square)];
			}
		}
		PawnEntry = PawnHash + (Next->pawn_key & pawn_hash_mask);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else {
		if (piece < WhiteKing) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		else {
			Next->pawn_key = Current->pawn_key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
			PawnEntry = PawnHash + (Next->pawn_key & pawn_hash_mask);
	        prefetch((char *)PawnEntry,_MM_HINT_NTA);
		}
		if (IsCastling(move)) {
			int rold, rnew;
			Next->ply = 0;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Add(mask_to,rnew);
			Square(rold) = 0;
			Square(rnew) = IRook(me);
			BB(IRook(me)) ^= Bit(rold);
			Piece(me) ^= Bit(rold);
			BB(IRook(me)) |= Bit(rnew);
			Piece(me) |= Bit(rnew);
			Next->pst += Pst(IRook(me),rnew) - Pst(IRook(me),rold);
			Next->key ^= PieceKey[IRook(me)][rnew] ^ PieceKey[IRook(me)][rold];
		}
	}

	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = opp;
	Next->key ^= TurnKey;
	Entry = Hash + (High32(Next->key) & hash_mask);
	prefetch((char *)Entry,_MM_HINT_NTA);
finish:
	sp++;
	Stack[sp] = Next->key;
	Next->move = move;
	Next->gen_flags = 0;
	Current++;
	nodes++;
}

template <bool me> void undo_move(int move) {
	int to, from, piece;
	from = From(move);
	to = To(move);
	if (IsPromotion(move)) {
		BB(Square(to)) ^= Bit(to);
		piece = IPawn(me);
	} else piece = Square(to);
	Square(from) = piece;
	BB(piece) |= Bit(from);
	Piece(me) |= Bit(from);
	BB(piece) &= ~Bit(to);
	Piece(me) ^= Bit(to);
	Square(to) = Current->capture;
	if (Current->capture) {
	    BB(Current->capture) |= Bit(to);
	    Piece(opp) |= Bit(to);
	} else {
		if (IsCastling(move)) {
			int rold, rnew;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Square(rnew) = 0;
			Square(rold) = IRook(me);
			Rook(me) ^= Bit(rnew);
			Piece(me) ^= Bit(rnew);
			Rook(me) |= Bit(rold);
			Piece(me) |= Bit(rold);
		} else if (IsEP(move)) {
			to = to ^ 8;
			piece = IPawn(opp);
			Square(to) = piece;
			Piece(opp) |= Bit(to);
			Pawn(opp) |= Bit(to);
		}
	}
	Current--;
	sp--;
}

void do_null() {
	GData * Next;
	GEntry * Entry;

	Next = Current + 1;
	Next->key = Current->key ^ TurnKey;
	Entry = Hash + (High32(Next->key) & hash_mask);
	prefetch((char *)Entry,_MM_HINT_NTA);
	Next->pawn_key = Current->pawn_key;
	Next->eval_key = 0;
	Next->turn = Current->turn ^ 1;
	Next->material = Current->material;
	Next->pst = Current->pst;
	Next->ply = 0;
	Next->castle_flags = Current->castle_flags;
	Next->ep_square = 0;
	Next->capture = 0;
	Next->piece_nb = Current->piece_nb; // TB
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	sp++;	
	Next->att[White] = Current->att[White];
	Next->att[Black] = Current->att[Black];
	Next->patt[White] = Current->patt[White];
	Next->patt[Black] = Current->patt[Black];
	Next->xray[White] = Current->xray[White];
	Next->xray[Black] = Current->xray[Black];
	Next->pin[White] = Current->pin[White];
	Next->pin[Black] = Current->pin[Black];
	Stack[sp] = Next->key;
	Next->threat = Current->threat;
	Next->passer = Current->passer;
	Next->score = -Current->score;
	Next->move = 0;
	Next->gen_flags = 0;
	Current++;
	nodes++;
}

void undo_null() {
	Current--;
	sp--;
}

template <bool me> int krbkrx() {
	if (King(opp) & Interior) return 1;
	return 16;
}
template <bool me> int kpkx() {
	uint64 u;
	if (me == White) u = Kpk[Current->turn][lsb(Pawn(White))][lsb(King(White))] & Bit(lsb(King(Black)));
	else u = Kpk[Current->turn ^ 1][63 - lsb(Pawn(Black))][63 - lsb(King(Black))] & Bit(63 - lsb(King(White)));
	if (u) return 32;
	else if (Piece(opp) ^ King(opp)) return 1;
	else return 0;
}
template <bool me> int knpkx() {
	if (Pawn(me) & Line(me, 6) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (SArea[sq] & King(opp) & (Line(me, 6) | Line(me, 7))) return 0;
		if (Square(sq + Push(me)) == IKing(me) && (SArea[lsb(King(me))] && SArea[lsb(King(opp))] & Line(me, 7))) return 0;
	} else if (Pawn(me) & Line(me, 5) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (Square(sq + Push(me)) == IPawn(opp)) {
			if (SArea[sq + Push(me)] & King(opp) & Line(me, 7)) return 0;
			if ((SArea[sq + Push(me)] & SArea[lsb(King(opp))] & Line(me, 7)) && (!(NAtt[sq + Push(me)] & Knight(me)) || Current->turn == opp)) return 0;
		}
	}
	return 32;
}
template <bool me> int krpkrx() {
	int mul = 32;
	int sq = lsb(Pawn(me));
	int rrank = CRank(me, sq);
	int o_king = lsb(King(opp));
	int o_rook = lsb(Rook(opp));
	int m_king = lsb(King(me));
	int add_mat = T(Piece(opp) ^ King(opp) ^ Rook(opp));
	int clear = F(add_mat) || F((PWay[opp][sq] | PIsolated[File(sq)]) & Forward[opp][Rank(sq + Push(me))] & (Piece(opp) ^ King(opp) ^ Rook(opp)));

	if (!clear) return 32;
	if (!add_mat && !(Pawn(me) & (File[0] | File[7]))) {
		int m_rook = lsb(Rook(me));
		if (CRank(me, o_king) < CRank(me, m_rook) && CRank(me, m_rook) < rrank && CRank(me, m_king) >= rrank - 1 && CRank(me, m_king) > CRank(me, m_rook)
			&& ((SArea[m_king] & Pawn(me)) || (Current->turn == me && Abs(File(sq) - File(m_king)) <= 1 && Abs(rrank - CRank(me, m_king)) <= 2))) return 128;
		if (SArea[m_king] & Pawn(me)) {
			if (rrank >= 4) {
				if ((File(sq) < File(m_rook) && File(m_rook) < File(o_king)) || (File(sq) > File(m_rook) && File(m_rook) > File(o_king))) return 128;
			} else if (rrank >= 2) {
				if (!(Pawn(me) & (File[1] | File[6])) && rrank + Abs(File(sq) - File(m_rook)) > 4
					&& ((File(sq) < File(m_rook) && File(m_rook) < File(o_king)) || (File(sq) > File(m_rook) && File(m_rook) > File(o_king)))) return 128;
			}
		}
	}

	if (PWay[me][sq] & King(opp)) {
		if (Pawn(me) & (File[0] | File[7])) mul = Min(mul, add_mat << 3);
		if (rrank <= 3) mul = Min(mul, add_mat << 3);
		if (rrank == 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5 && T(King(opp) & (Line(me, 6) | Line(me, 7)))
			&& (Current->turn != me || F(PAtt[me][sq] & RookAttacks(lsb(Rook(me)), PieceAll) & (~SArea[o_king])))) mul = Min(mul, add_mat << 3);
		if (rrank >= 5 && CRank(me, o_rook) <= 1 && (Current->turn != me || Check(me) || Dist(m_king, sq) >= 2)) mul = Min(mul, add_mat << 3);
		if (T(King(opp) & (File[1] | File[2] | File[6] | File[7])) && T(Rook(opp) & Line(me, 7)) && T(Between[o_king][o_rook] & (File[3] | File[4])) && F(Rook(me) & Line(me, 7))) mul = Min(mul, add_mat << 3);
		return mul;
	} else if (rrank == 6 && (Pawn(me) & (File[0] | File[7])) && ((PSupport[me][sq] | PWay[opp][sq]) & Rook(opp)) && CRank(me, o_king) >= 6) {
		int dist = Abs(File(sq) - File(o_king));
		if (dist <= 3)  mul = Min(mul, add_mat << 3);
		if (dist == 4 && ((PSupport[me][o_king] & Rook(me)) || Current->turn == opp)) mul = Min(mul, add_mat << 3);
	}

	if (SArea[o_king] & PWay[me][sq] & Line(me, 7)) {
		if (rrank <= 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5) mul = Min(mul, add_mat << 3);
		if (rrank == 5 && CRank(me, o_rook) <= 1 && Current->turn != me || (F(SArea[m_king] & PAtt[me][sq] & (~SArea[o_king])) && (Check(me) || Dist(m_king, sq) >= 2)))
			mul = Min(mul, add_mat << 3);
	}

	if (T(PWay[me][sq] & Rook(me)) && T(PWay[opp][sq] & Rook(opp))) {
		if (King(opp) & (File[0] | File[1] | File[6] | File[7]) & Line(me, 6)) mul = Min(mul, add_mat << 3);
		else if ((Pawn(me) & (File[0] | File[7])) && (King(opp) & (Line(me, 5) | Line(me, 6))) && Abs(File(sq) - File(o_king)) <= 2 && File(sq) != File(o_king)) mul = Min(mul, add_mat << 3);
	}

	if (Abs(File(sq) - File(o_king)) <= 1 && Abs(File(sq) - File(o_rook)) <= 1 && CRank(me, o_rook) > rrank && CRank(me, o_king) > rrank) mul = Min(mul, (Pawn(me) & (File[3] | File[4])) ? 12 : 16);

	return mul;
}
template <bool me> int krpkbx() {
	if (!(Pawn(me) & Line(me, 5))) return 32;
	int sq = lsb(Pawn(me));
	if (!(PWay[me][sq] & King(opp))) return 32;
	int diag_sq = NB(me, BMask[sq + Push(me)]);
	if (CRank(me, diag_sq) > 1) return 32;
	uint64 mdiag = FullLine[sq + Push(me)][diag_sq] | Bit(sq + Push(me)) | Bit(diag_sq);
	int check_sq = NB(me, BMask[sq - Push(me)]);
	uint64 cdiag = FullLine[sq - Push(me)][check_sq] | Bit(sq - Push(me)) | Bit(check_sq);
	if ((mdiag | cdiag) & (Piece(opp) ^ King(opp) ^ Bishop(opp))) return 32;
	if (cdiag & Bishop(opp)) return 0;
	if ((mdiag & Bishop(opp)) && (Current->turn == opp || !(King(me) & PAtt[opp][sq + Push(me)]))) return 0;
	return 32;
}
template <bool me> int kqkp() {
	if (F(SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1) & (File[0] | File[2] | File[5] | File[7]))) return 32;
	if (PWay[opp][lsb(Pawn(opp))] & (King(me) | Queen(me))) return 32;
	if (Pawn(opp) & (File[0] | File[7])) return 1;
	else return 4;
}
template <bool me> int kqkrpx() {
	int rsq = lsb(Rook(opp));
	uint64 pawns = SArea[lsb(King(opp))] & PAtt[me][rsq] & Pawn(opp) & Interior & Line(me, 6);
	if (pawns && CRank(me, lsb(King(me))) <= 4) return 0;
	return 32;
}
template <bool me> int krkpx() {
	if (T(SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1)) & F(PWay[opp][NB(me, Pawn(opp))] & King(me))) return 0;
	return 32;
}
template <bool me> int krppkrpx() {
	if (Current->passer & Pawn(me)) {
		if (Single(Current->passer & Pawn(me))) {
			int sq = lsb(Current->passer & Pawn(me));
			if (PWay[me][sq] & King(opp) & (File[0] | File[1] | File[6] | File[7])) {
				int opp_king = lsb(King(opp));
				if (SArea[opp_king] & Pawn(opp)) {
					int king_file = File(opp_king);
					if (!((~(File[king_file] | PIsolated[king_file])) & Pawn(me))) return 1;
				}
			}
		}
		return 32;
	}
	if (F((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int krpppkrppx() {
	if (T(Current->passer & Pawn(me)) || F((SArea[lsb(Pawn(opp))] | SArea[msb(Pawn(opp))]) & Pawn(opp))) return 32;
	if (F((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int kbpkbx() {
	int sq = lsb(Pawn(me));
	uint64 u;
	if ((T(Board->bb[ILight(me)]) && T(Board->bb[IDark(opp)])) || (T(Board->bb[IDark(me)]) && T(Board->bb[ILight(opp)]))) {
		if (CRank(me, sq) <= 4) return 0;
		if (T(PWay[me][sq] & King(opp)) && CRank(me, sq) <= 5) return 0;
		for (u = Bishop(opp); T(u); Cut(u)) {
			if (CRank(me, lsb(u)) <= 4 && T(BishopAttacks(lsb(u), PieceAll) & PWay[me][sq])) return 0;
			if (Current->turn == opp && T(BishopAttacks(lsb(u), PieceAll) & Pawn(me))) return 0;
		}
	} else if (T(PWay[me][sq] & King(opp)) && T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) return 0;
	return 32;
}
template <bool me> int kbpknx() {
	uint64 u;
	if (T(PWay[me][lsb(Pawn(me))] & King(opp)) && T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) return 0;
	if (Current->turn == opp)
	for (u = Knight(opp); T(u); Cut(u))
	if (NAtt[lsb(u)] & Pawn(me)) return 0;
	return 32;
}
template <bool me> int kbppkbx() {
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));
	int o_king = lsb(King(opp));
	int o_bishop = lsb(Bishop(opp));

	if (File(sq1) == File(sq2)) {
		if (CRank(me, sq2) <= 3) return 0;
		if (T(PWay[me][sq2] & King(opp)) && CRank(me, sq2) <= 5) return 0;
	} else if (PIsolated[File(sq1)] & Pawn(me)) {
		if (T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) {
			if (T((SArea[o_king] | King(opp)) & Bit(sq2 + Push(me))) && T(BishopAttacks(o_bishop, PieceAll) & Bit(sq2 + Push(me))))
			if (T((SArea[o_king] | King(opp)) & Bit((sq2 & 0xFFFFFFF8) | File(sq1))) && T(BishopAttacks(o_bishop, PieceAll) & Bit((sq2 & 0xFFFFFFF8) | File(sq1)))) return 0;
		}
	}
	return 32;
}
template <bool me> int krppkrx() {
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));

	if ((Piece(opp) ^ King(opp) ^ Rook(opp)) & Forward[me][Rank(sq1 - Push(me))]) return 32;
	if (File(sq1) == File(sq2)) {
		if (T(PWay[me][sq2] & King(opp))) return 16;
		return 32;
	}
	if (T(PIsolated[File(sq2)] & Pawn(me)) && T((File[0] | File[7]) & Pawn(me)) && T(King(opp) & Shift(me, Pawn(me)))) {
		if (CRank(me, sq2) == 5 && CRank(me, sq1) == 4 && T(Rook(opp) & (Line(me, 5) | Line(me, 6)))) return 10;
		else if (CRank(me, sq2) < 5) return 16;
	}
	return 32;
}
typedef struct {
	int king_w, king_b, score;
	uint64 patt_w, patt_b, double_att_w, double_att_b;
} GPawnEvalInfo;

template <bool me, bool HPopCnt> __forceinline void eval_pawns(GPawnEntry * PawnEntry, GPawnEvalInfo &PEI) {
	int kf = File(PVarC(PEI, king, me));
	int kr = Rank(PVarC(PEI, king, me));
	int start, inc;
	if (kf <= 3) {
		start = Max(kf - 1, 0);
		inc = 1;
	} else {
		start = Min(kf + 1, 7);
		inc = -1;
	}
	int shelter = 0;
	uint64 mpawns = Pawn(me) & Forward[me][me ? Min(kr + 1, 7) : Max(kr - 1, 0)];
	for (int file = start, i = 0; i < 3; file += inc, i++) {
		shelter += Shelter[i][CRank(me, NBZ(me, mpawns & File[file]))];
		int rank;
		if (Pawn(opp) & File[file]) {
			int sq = NB(me, Pawn(opp) & File[file]);
			if ((rank = CRank(opp, sq)) < 6) {
				if (rank >= 3) shelter += StormBlocked[rank - 3];
				if (uint64 u = (PIsolated[File(sq)] & Forward[opp][Rank(sq)] & Pawn(me))) {
					int square = NB(opp, u);
					uint64 att_sq = PAtt[me][square] & PWay[opp][sq]; // may be zero
					if ((File[File(square)] | PIsolated[File(square)]) & King(me)) if (!(PVarC(PEI, double_att, me) & att_sq) || (Current->patt[opp] & att_sq)) {
						if (PWay[opp][square] & Pawn(me)) continue;
						if (!(PawnAll & PWay[opp][sq] & Forward[me][Rank(square)])) {
							if (rank >= 3) {
								shelter += StormShelterAtt[rank - 3];
								if (PVarC(PEI, patt, opp) & Bit(sq + Push(opp))) shelter += StormConnected[rank - 3];
								if (!(PWay[opp][sq] & PawnAll)) shelter += StormOpen[rank - 3];
							}
							if (!((File[File(sq)] | PIsolated[File(sq)]) & King(opp)) && rank <= 4) shelter += StormFree[rank - 1];
						}
					}
				}
			}
		} else {
			shelter += Sa(StormHof, StormHofValue);
			if (!(Pawn(me) & File[file])) shelter += Sa(StormHof, StormOfValue);
		}
	}
	PawnEntry->shelter[me] = shelter;

	uint64 b;
	int min_file = 7, max_file = 0;
	for (uint64 u = Pawn(me); T(u); u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		int rank = Rank(sq);
		int rrank = CRank(me, sq);
		int file = File(sq);
		uint64 way = PWay[me][sq];
		int next = Square(sq + Push(me));
		if (file < min_file) min_file = file;
		if (file > max_file) max_file = file;

		int isolated = !(Pawn(me) & PIsolated[file]);
		int doubled = T(Pawn(me) & (File[file] ^ b));
		int open = !(PawnAll & way);
		int up = !(PVarC(PEI, patt, me) & b);

		if (isolated) {
			if (open) DecV(PEI.score, Ca(Isolated, IsolatedOpen));
			else {
				DecV(PEI.score, Ca(Isolated, IsolatedClosed));
				if (next == IPawn(opp)) DecV(PEI.score, Ca(Isolated, IsolatedBlocked));
			}
			if (doubled) {
				if (open) DecV(PEI.score, Ca(Isolated, IsolatedDoubledOpen));
				else DecV(PEI.score, Ca(Isolated, IsolatedDoubledClosed));
			}
		} else {
			if (doubled) {
				if (open) DecV(PEI.score, Ca(Doubled, DoubledOpen));
				else DecV(PEI.score, Ca(Doubled, DoubledClosed));
			}
			if (rrank >= 3 && (b & (File[2] | File[3] | File[4] | File[5])) && next != IPawn(opp) && (PIsolated[file] & Line[rank] & Pawn(me)))
				IncV(PEI.score, Ca(PawnSpecial, PawnChainLinear) * (rrank - 3) + Ca(PawnSpecial, PawnChain));
		}
		int backward = 0;
		if (!(PSupport[me][sq] & Pawn(me))) {
			if (isolated) backward = 1;
			else if (uint64 v = (PawnAll | PVarC(PEI, patt, opp)) & way) if (IsGreater(me, NB(me, PVarC(PEI, patt, me) & way), NB(me, v))) backward = 1;
		}
		if (backward) {
			if (open) DecV(PEI.score, Ca(Backward, BackwardOpen));
			else DecV(PEI.score, Ca(Backward, BackwardClosed));
		} else if (open) if (!(Pawn(opp) & PIsolated[file]) || popcount<HPopCnt>(Pawn(me) & PIsolated[file]) >= popcount<HPopCnt>(Pawn(opp) & PIsolated[file])) IncV(PEI.score,PasserCandidate[rrank]); // IDEA: more precise pawn counting for the case of, say, white e5 candidate with black pawn on f5 or f4...
		if (up && next == IPawn(opp)) {
			DecV(PEI.score, Ca(Unprotected, UpBlocked));
			if (backward) {
				if (rrank <= 2) { // IDEA (based on weird passer target tuning result): may be score unprotected/backward depending on rank/file?
					DecV(PEI.score, Ca(Unprotected, PasserTarget));
					if (rrank <= 1) DecV(PEI.score, Ca(Unprotected, PasserTarget));
				}
				for (uint64 v = PAtt[me][sq] & Pawn(me); v; Cut(v)) if ((PSupport[me][lsb(v)] & Pawn(me)) == b) {
					DecV(PEI.score, Ca(Unprotected, ChainRoot));
					break;
				}
			}
		}
		if (open && !(PIsolated[file] & Forward[me][rank] & Pawn(opp))) {
			PawnEntry->passer[me] |= (uint8)(1 << file);
			if (rrank <= 2) continue;
			IncV(PEI.score, PasserGeneral[rrank]);
			int dist_att = Dist(PVarC(PEI, king, opp), sq + Push(me)); // IDEA: average the distance with the distance to the promotion square? or just use the latter?
			int dist_def = Dist(PVarC(PEI, king, me), sq + Push(me));
			IncV(PEI.score, Compose256(0, dist_att * (int)PasserAtt[rrank] + LogDist[dist_att] * (int)PasserAttLog[rrank] - dist_def * (int)PasserDef[rrank] - (int)LogDist[dist_def] * (int)PasserDefLog[rrank]));
			if (PVarC(PEI, patt, me) & b) IncV(PEI.score, PasserProtected[rrank]);
			if (!(Pawn(opp) & West[file]) || !(Pawn(opp) & East[file])) IncV(PEI.score, PasserOutside[rrank]);
		}
	}
	uint64 files = 0;
	for (int i = 1; i < 7; i++) files |= (Pawn(me) >> (i << 3)) & 0xFF;
	int file_span = (files ? (msb(files) - lsb(files)) : 0);
	IncV(PEI.score, Ca(PawnSpecial, PawnFileSpan) * file_span);
	PawnEntry->draw[me] = (7 - file_span) * Max(5 - popcount<HPopCnt>(files), 0);
}

template <bool HPopCnt> void eval_pawn_structure(GPawnEntry * PawnEntry) {
	GPawnEvalInfo PEI;
	for (int i = 0; i < sizeof(GPawnEntry) / sizeof(int); i++) *(((int*)PawnEntry) + i) = 0;
	PawnEntry->key = Current->pawn_key;

	PEI.patt_w = ShiftW(White, Pawn(White)) | ShiftE(White, Pawn(White));
	PEI.patt_b = ShiftW(Black, Pawn(Black)) | ShiftE(Black, Pawn(Black));
	PEI.double_att_w = ShiftW(White, Pawn(White)) & ShiftE(White, Pawn(White));
	PEI.double_att_b = ShiftW(Black, Pawn(Black)) & ShiftE(Black, Pawn(Black));
	PEI.king_w = lsb(King(White));
	PEI.king_b = lsb(King(Black));
	PEI.score = 0;

	eval_pawns<White, HPopCnt>(PawnEntry, PEI);
	eval_pawns<Black, HPopCnt>(PawnEntry, PEI);

	PawnEntry->score = PEI.score;
}

typedef struct {
	int score, king_w, king_b, mul;
	uint64 occ, area_w, area_b, free_w, free_b;
	uint32 king_att_w, king_att_b;
	GPawnEntry * PawnEntry;
	GMaterial * material;
} GEvalInfo;

template <bool me, bool HPopCnt> __forceinline void eval_queens(GEvalInfo &EI) {
	uint64 u, b;
	for (u = Queen(me); T(u); u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64 att = QueenAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (QMask[sq] & King(opp)) if (uint64 v = Between[PVarC(EI,king,opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64 square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp) && !(((BMask[sq] & Bishop(opp)) | (RMask[sq] & Rook(opp)) | Queen(opp)) & v)) {
				IncV(EI.score, Ca(Pin, WeakPin));
				if (!(Current->patt[opp] & v)) katt = 1;
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & Minor(opp))) IncV(EI.score, Ca(KingRay, QKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingQAttack;
			for (uint64 v = att & PVarC(EI, area, opp); T(v); Cut(v))
			if (FullLine[sq][lsb(v)] & att & ((Rook(me) & RMask[sq]) | (Bishop(me) & BMask[sq]))) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteQueen] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefQueen));
	}
}
template <bool me, bool HPopCnt> __forceinline void eval_rooks(GEvalInfo &EI) {
	uint64 u, b;
	for (u = Rook(me); T(u); u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64 att = RookAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (RMask[sq] & King(opp)) if (uint64 v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64 square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < IRook(opp)) {
					IncV(EI.score, Ca(Pin, WeakPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece == IQueen(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Minor(opp) | Queen(opp)))) IncV(EI.score, Ca(KingRay, RKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingRAttack;
			for (uint64 v = att & PVarC(EI, area, opp); T(v); Cut(v))
			if (FullLine[sq][lsb(v)] & att & Major(me)) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteRook] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefRook));
		Current->threat |= att & Queen(opp);
		if (!(PWay[me][sq] & Pawn(me))) {
			IncV(EI.score, Ca(RookSpecial, RookHof));
			int hof_score = 0;
			if (!(PWay[me][sq] & Pawn(opp))) {
				IncV(EI.score, Ca(RookSpecial, RookOf));
				if (att & Line(me, 7)) hof_score += Ca(RookSpecial, RookOfOpen);
				else if (uint64 target = att & PWay[me][sq] & Minor(opp)) {
					if (!(Current->patt[opp] & target)) {
						hof_score += Ca(RookSpecial, RookOfMinorHaging);
						if (PWay[me][sq] & King(opp)) hof_score += Ca(RookSpecial, RookOfKingAtt);
					} else hof_score += Ca(RookSpecial, RookOfMinorFixed);
				}
			} else if (att & PWay[me][sq] & Pawn(opp)) {
				uint64 square = lsb(att & PWay[me][sq] & Pawn(opp));
				if (!(PSupport[opp][square] & Pawn(opp))) hof_score += Ca(RookSpecial, RookHofWeakPAtt);
			}
			IncV(EI.score, hof_score);
			if (PWay[opp][sq] & att & Major(me)) IncV(EI.score, hof_score);
		}
		if ((b & Line(me, 6)) && ((King(opp) | Pawn(opp)) & (Line(me, 6) | Line(me, 7)))) {
			IncV(EI.score, Ca(RookSpecial, Rook7th));
			if (King(opp) & Line(me, 7)) IncV(EI.score, Ca(RookSpecial, Rook7thK8th));
			if (Major(me) & att & Line(me, 6)) IncV(EI.score, Ca(RookSpecial, Rook7thDoubled));
		}
	}
}
template <bool me, bool HPopCnt> __forceinline void eval_bishops(GEvalInfo &EI) {
	uint64 u, b;
	for (u = Bishop(me); T(u); u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64 att = BishopAttacks(sq, EI.occ);
		Current->att[me] |= att;
		if (BMask[sq] & King(opp)) if (uint64 v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64 square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < ILight(opp)) {
					IncV(EI.score, Ca(Pin, StrongPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece >= IRook(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Knight(opp) | Major(opp)))) IncV(EI.score, Ca(KingRay, BKingRay));
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingBAttack;
		IncV(EI.score, Mobility[PieceType[WhiteLight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Knight(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefBishop));
		Current->threat |= att & Major(opp);
		if (b & LightArea) {
			for (uint64 v = ((~BishopForward[me][sq]) | (att & Forward[me][Rank(sq)])) & Pawn(opp) & (~Current->patt[opp]) & LightArea; v; Cut(v)) {
				uint64 square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64 v = BishopForward[me][sq] & Pawn(me) & LightArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		} else {
			for (uint64 v = ((~BishopForward[me][sq]) | (att & Forward[me][Rank(sq)])) & Pawn(opp) & (~Current->patt[opp]) & DarkArea; v; Cut(v)) {
				uint64 square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64 v = BishopForward[me][sq] & Pawn(me) & DarkArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		}
	}
}
template <bool me, bool HPopCnt> __forceinline void eval_knights(GEvalInfo &EI) {
	uint64 u, b;
	for (u = Knight(me); T(u); u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64 att = NAtt[sq];
		Current->att[me] |= att;
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingNAttack;
		IncV(EI.score, Mobility[PieceType[WhiteKnight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefKnight));
		Current->threat |= att & Major(opp);
		if ((b & Outpost[me]) && !(Pawn(opp) & PIsolated[File(sq)] & Forward[me][Rank(sq)])) {
			IncV(EI.score, Ca(KnightSpecial, KnightOutpost));
			if (Current->patt[me] & b) {
				IncV(EI.score, Ca(KnightSpecial, KnightOutpostProtected));
				if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostPawnAtt));
				if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostBishopAtt));
			}
		}
	}
}
template <bool me, bool HPopCnt> __forceinline void eval_king(GEvalInfo &EI) {
	int cnt = Opening(PVarC(EI, king_att, me));
	int score = Endgame(PVarC(EI, king_att, me));
	if (cnt >= 2 && T(Queen(me))) {
		score += (EI.PawnEntry->shelter[opp] * KingShelterQuad)/64;
		if (uint64 u = Current->att[me] & PVarC(EI, area, opp) & (~Current->att[opp])) score += popcount<HPopCnt>(u) * KingAttackSquare;
		if (!(SArea[PVarC(EI, king, opp)] & (~(Piece(opp) | Current->att[me])))) score += KingNoMoves;
	}
	int adjusted = ((score * KingAttackScale[cnt]) >> 3) + EI.PawnEntry->shelter[opp];
	if (!Queen(me)) adjusted /= 2;
	IncV(EI.score, adjusted);
}
template <bool me, bool HPopCnt> __forceinline void eval_passer(GEvalInfo &EI) {
	for (uint64 u = EI.PawnEntry->passer[me]; T(u); Cut(u)) {
		int file = lsb(u);
		int sq = NB(opp, File[file] & Pawn(me));
		int rank = CRank(me, sq);
		Current->passer |= Bit(sq);
		if (rank <= 2) continue;
		if (!Square(sq + Push(me))) IncV(EI.score, PasserBlocked[rank]);
		uint64 way = PWay[me][sq];
		int connected = 0, supported = 0, hooked = 0, unsupported = 0, free = 0;
		if (!(way & Piece(opp))) {
			IncV(EI.score, PasserClear[rank]);
			if (PWay[opp][sq] & Major(me)) {
				int square = NB(opp, PWay[opp][sq] & Major(me));
				if (F(Between[sq][square] & EI.occ)) supported = 1;
			}
			if (PWay[opp][sq] & Major(opp)) {
				int square = NB(opp, PWay[opp][sq] & Major(opp));
				if (F(Between[sq][square] & EI.occ)) hooked = 1;
			}
			for (uint64 v = PAtt[me][sq - Push(me)] & Pawn(me); T(v); Cut(v)) {
				int square = lsb(v);
				if (F(Pawn(opp) & (File[File(square)] | PIsolated[File(square)]) & Forward[me][Rank(square)])) connected++;
			}
			if (connected) IncV(EI.score, PasserConnected[rank]);
			if (!hooked && !(Current->att[opp] & way)) {
				IncV(EI.score, PasserFree[rank]);
				free = 1;
			} else {
				uint64 attacked = Current->att[opp] | (hooked ? way : 0);
				if (supported || (!hooked && connected) || (!(Major(me) & way) && !(attacked & (~Current->att[me])))) IncV(EI.score, PasserSupported[rank]);
				else unsupported = 1;
			}
		}
		if (rank == 6) {
			if ((way & Rook(me)) && !Minor(me) && !Queen(me) && Single(Rook(me))) DecV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpRookBlock)));
			if (!Major(opp) && (!NonPawnKing(opp) || Single(NonPawnKing(opp)))) {
				IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOnePiece)));
				if (!free) {
					if (!(SArea[sq + Push(me)] & King(opp))) IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpMinorControl)));
					else IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpKingControl)));
				}
			}
		}
	}
}
template <bool me, bool HPopCnt> __forceinline void eval_pieces(GEvalInfo &EI) {
	Current->threat |= Current->att[opp] & (~Current->att[me]) & Piece(me);
	if (uint64 u = Current->threat & Piece(me)) {
		DecV(EI.score, Ca(Tactical, TacticalThreat));
		Cut(u);
		if (u) {
			DecV(EI.score, Ca(Tactical, TacticalThreat) + Ca(Tactical, TacticalDoubleThreat));
			for (Cut(u); u; Cut(u)) DecV(EI.score, Ca(Tactical, TacticalThreat));
		}
	}
}
template <bool me, bool HPopCnt> void eval_endgame(GEvalInfo &EI) {
	if ((EI.material->flags & VarC(FlagSingleBishop, me)) && Pawn(me)) {
		int sq = (Board->bb[ILight(me)] ? (me ? 0 : 63) : (Board->bb[IDark(me)] ? (me ? 7 : 56) : (File(lsb(King(opp))) <= 3 ? (me ? 0 : 56) : (me ? 7 : 63))));
		if (!(Pawn(me) & (~PWay[opp][sq]))) {
			if ((SArea[sq] | Bit(sq)) & King(opp)) EI.mul = 0;
			else if ((SArea[sq] & SArea[lsb(King(opp))] & Line(me, 7)) && Square(sq - Push(me)) == IPawn(opp) && Square(sq - 2 * Push(me)) == IPawn(me)) EI.mul = 0;
		} else if ((King(opp) & Line(me, 6) | Line(me, 7)) && Abs(File(sq) - File(lsb(King(opp)))) <= 3 && !(Pawn(me) & (~PSupport[me][sq])) && (Pawn(me) & Line(me, 5) & Shift(opp, Pawn(opp)))) EI.mul = 0;
		if (Single(Pawn(me))) {
			if (!Bishop(me)) {
				EI.mul = MinF(EI.mul, kpkx<me>());
				if (Piece(opp) == King(opp) && EI.mul == 32) IncV(Current->score, KpkValue);
			} else {
				sq = lsb(Pawn(me));
				if ((Pawn(me) & (File[1] | File[6]) & Line(me, 5)) && Square(sq + Push(me)) == IPawn(opp) && ((PAtt[me][sq + Push(me)] | PWay[me][sq + Push(me)]) & King(opp))) EI.mul = 0;
			}
		}
		if (Bishop(opp) && Single(Bishop(opp)) && T(BB(ILight(me))) != T(BB(ILight(opp)))) {
			int pcnt = 0;
			if (T(King(opp) & LightArea) == T(Bishop(opp) & LightArea)) {
				for (uint64 u = Pawn(me); u; Cut(u)) {
					if (pcnt >= 2) goto check_for_partial_block;
					pcnt++;
					int sq = lsb(u);
					if (!(PWay[me][sq] & (PAtt[me][PVarC(EI, king, opp)] | PAtt[opp][PVarC(EI, king, opp)]))) {
						if (!(PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						int bsq = lsb(Bishop(opp));
						uint64 att = BishopAttacks(bsq, EI.occ);
						if (!(att & PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						if (!(BishopForward[me][bsq] & att & PWay[me][sq] & Pawn(opp)) && popcount<HPopCnt>(FullLine[lsb(att & PWay[me][sq] & Pawn(opp))][bsq] & att) <= 2)  goto check_for_partial_block;
					}
				}
				EI.mul = 0;
				return;
			}
		check_for_partial_block:
			if (pcnt <= 2 && Multiple(Pawn(me)) && !Pawn(opp) && !(Pawn(me) & Boundary) && EI.mul) {
				int sq1 = lsb(Pawn(me));
				int sq2 = msb(Pawn(me));
				int fd = Abs(File(sq2) - File(sq1));
				if (fd >= 5) EI.mul = 32;
				else if (fd >= 4) EI.mul = 26;
				else if (fd >= 3) EI.mul = 20;
			}
			if ((SArea[PVarC(EI, king, opp)] | Current->patt[opp]) & Bishop(opp)) {
				uint64 push = Shift(me, Pawn(me));
				if (!(push & (~(Piece(opp) | Current->att[opp]))) && (King(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea))) {
					EI.mul = Min(EI.mul, 8);
					int bsq = lsb(Bishop(opp));
					uint64 att = BishopAttacks(bsq, EI.occ);
					uint64 prp = (att | SArea[PVarC(EI, king, opp)]) & Pawn(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea);
					uint64 patt = ShiftW(opp, prp) | ShiftE(opp, prp);
					if ((SArea[PVarC(EI, king, opp)] | patt) & Bishop(opp)) {
						uint64 double_att = (SArea[PVarC(EI, king, opp)] & patt) | (patt & att) | (SArea[PVarC(EI, king, opp)] & att);
						if (!(push & (~(King(opp) | Bishop(opp) | prp | double_att)))) {
							EI.mul = 0;
							return;
						}
					}
				}
			}
		}
	}
	if (F(Major(me))) {
		if (T(Bishop(me)) && F(Knight(me)) && Single(Bishop(me)) && T(Pawn(me))) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number == 1) {
				if (Bishop(opp)) EI.mul = MinF(EI.mul, kbpkbx<me>());
				else if (Knight(opp)) EI.mul = MinF(EI.mul, kbpknx<me>());
			} else if (number == 2 && T(Bishop(opp))) EI.mul = MinF(EI.mul, kbppkbx<me>());
		} else if (!Bishop(me) && Knight(me) && Single(Knight(me)) && Pawn(me) && Single(Pawn(me))) EI.mul = MinF(EI.mul, knpkx<me>());
	} else if (F(Minor(me))) {
		if (F(Pawn(me)) && F(Rook(me)) && T(Queen(me)) && T(Pawn(opp))) {
			if (F(NonPawnKing(opp)) && Single(Pawn(opp))) EI.mul = MinF(EI.mul, kqkp<me>());
			else if (Rook(opp)) EI.mul = MinF(EI.mul, kqkrpx<me>());
		} else if (F(Queen(me)) && T(Rook(me)) && Single(Rook(me))) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number <= 3) {
				if (number == 0) {
					if (Pawn(opp)) EI.mul = MinF(EI.mul, krkpx<me>());
				} else if (Rook(opp)) {
					if (number == 1) {
						int new_mul = krpkrx<me>();
						EI.mul = (new_mul <= 32 ? Min(EI.mul, new_mul) : new_mul);
					} else {
						if (number == 2) EI.mul = MinF(EI.mul, krppkrx<me>());
						if (Pawn(opp)) {
							if (number == 2) EI.mul = MinF(EI.mul, krppkrpx<me>());
							else if (Multiple(Pawn(opp))) EI.mul = MinF(EI.mul, krpppkrppx<me>());
						}
					}
				} else if (number == 1 && Bishop(opp)) EI.mul = MinF(EI.mul, krpkbx<me>());
			}
		}
	} else if (!Pawn(me) && Single(Rook(me)) && !Queen(me) && Single(Bishop(me)) && !Knight(me) && Rook(opp)) EI.mul = MinF(EI.mul, krbkrx<me>());
	if (F(NonPawnKing(opp)) && Current->turn == opp && F(Current->att[me] & King(opp)) && !(SArea[PVarC(EI, king, opp)] & (~(Current->att[me] | Piece(opp))))
		&& F(Current->patt[opp] & Piece(me)) && F(Shift(opp, Pawn(opp)) & (~EI.occ)))
		EI.mul = 0;
}
template <bool HPopCnt> void eval_unusual_material(GEvalInfo &EI) {
	int wp, bp, wlight, blight, wr, br, wq, bq;
	wp = popcount<HPopCnt>(Pawn(White));
	bp = popcount<HPopCnt>(Pawn(Black));
	wlight = popcount<HPopCnt>(Minor(White));
	blight = popcount<HPopCnt>(Minor(Black));
	wr = popcount<HPopCnt>(Rook(White));
	br = popcount<HPopCnt>(Rook(Black));
	wq = popcount<HPopCnt>(Queen(White));
	bq = popcount<HPopCnt>(Queen(Black));
	int phase = Min(24, (wlight + blight) + 2 * (wr + br) + 4 * (wq + bq));
	int mat_score = SeeValue[WhitePawn] * (wp - bp) + SeeValue[WhiteKnight] * (wlight - blight) + SeeValue[WhiteRook] * (wr - br) + SeeValue[WhiteQueen] * (wq - bq);
	mat_score = Compose(mat_score,mat_score);
	Current->score = (((Opening(mat_score + EI.score) * phase) + (Endgame(mat_score + EI.score) * (24 - phase)))/24);
	if (Current->turn) Current->score = -Current->score;
	UpdateDelta
}

template <bool HPopCnt> void evaluation() {
	GEvalInfo EI;
	
	if (Current->eval_key == Current->key) return;
	Current->eval_key = Current->key;

	EI.king_w = lsb(King(White));
	EI.king_b = lsb(King(Black));
	EI.occ = PieceAll;
	Current->patt[White] = ShiftW(White,Pawn(White)) | ShiftE(White,Pawn(White));
	Current->patt[Black] = ShiftW(Black,Pawn(Black)) | ShiftE(Black,Pawn(Black));
	EI.area_w = (SArea[EI.king_w] | King(White)) & ((~Current->patt[White]) | Current->patt[Black]);
	EI.area_b = (SArea[EI.king_b] | King(Black)) & ((~Current->patt[Black]) | Current->patt[White]);
	Current->att[White] = Current->patt[White];
	Current->att[Black] = Current->patt[Black];
	Current->passer = 0;
	Current->threat = (Current->patt[White] & NonPawn(Black)) | (Current->patt[Black] & NonPawn(White));
	EI.score = Current->pst;

#define me White
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me
#define me Black
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me

	EI.PawnEntry = PawnHash + (Current->pawn_key & pawn_hash_mask);
	if (Current->pawn_key != EI.PawnEntry->key) eval_pawn_structure<HPopCnt>(EI.PawnEntry);
	EI.score += EI.PawnEntry->score;

	eval_king<White, HPopCnt>(EI);
	eval_king<Black, HPopCnt>(EI);
	Current->att[White] |= SArea[EI.king_w];
	Current->att[Black] |= SArea[EI.king_b];

	eval_passer<White, HPopCnt>(EI);
	eval_pieces<White, HPopCnt>(EI);
	eval_passer<Black, HPopCnt>(EI);
	eval_pieces<Black, HPopCnt>(EI);

	if (Current->material & FlagUnusualMaterial) {
		eval_unusual_material<HPopCnt>(EI);
		return;
	}
	EI.material = &Material[Current->material];

	Current->score = EI.material->score + (((Opening(EI.score) * EI.material->phase) + (Endgame(EI.score) * (128 - (int)EI.material->phase)))/128);

	if (Current->ply >= 50) Current->score /= 2;
	if (Current->score > 0) {
		EI.mul = EI.material->mul[White];
		if (EI.material->flags & FlagCallEvalEndgame_w) eval_endgame<White, HPopCnt>(EI);
		Current->score -= (Min(Current->score, 100) * (int)EI.PawnEntry->draw[White]) / 64;
	} else if (Current->score < 0) {
		EI.mul = EI.material->mul[Black];
		if (EI.material->flags & FlagCallEvalEndgame_b) eval_endgame<Black, HPopCnt>(EI);
		Current->score += (Min(-Current->score, 100) * (int)EI.PawnEntry->draw[Black]) / 64;
	} else EI.mul = Min(EI.material->mul[White], EI.material->mul[Black]);
	Current->score = (Current->score * EI.mul)/32;

	if (Current->turn) Current->score = -Current->score;
	UpdateDelta
}

__forceinline void evaluate() {
	HardwarePopCnt ? evaluation<1>() : evaluation<0>();
}

template <bool me> int is_legal(int move) {
	int from, to, piece, capture;
	uint64 u, occ;

    from = From(move);
	to = To(move);
	piece = Board->square[from];
	capture = Board->square[to];
	if (piece == 0) return 0;
	if ((piece & 1) != Current->turn) return 0;
	if (capture) {
		if ((capture & 1) == (piece & 1)) return 0;
		if (capture >= WhiteKing) return 0;
	}
	occ = PieceAll;
	u = Bit(to);
	if (piece >= WhiteLight && piece < WhiteKing) {
	    if ((QMask[from] & u) == 0) return 0;
		if (Between[from][to] & occ) return 0;
	}
	if (IsEP(move)) {
		if (piece >= WhiteKnight) return 0;
		if (Current->ep_square != to) return 0;
		return 1;
	}
	if (IsCastling(move) && Board->square[from] < WhiteKing) return 0;
	if (IsPromotion(move) && Board->square[from] >= WhiteKnight) return 0;
	if (piece == IPawn(me)) {
		if (u & PMove[me][from]) {
            if (capture) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else if (to == (from + 2 * Push(me))) {
            if (capture) return 0;
			if (Square(to - Push(me))) return 0;
			if (F(u & Line(me,3))) return 0;
			return 1;
		} else if (u & PAtt[me][from]) {
            if (capture == 0) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else return 0;
	} else if (piece == IKing(me)) {
		if (me == White) {
		    if (IsCastling(move)) {
			    if (u & 0x40) {
                    if (((Current->castle_flags) & CanCastle_OO) == 0) return 0;
					if (occ & 0x60) return 0;
					if (Current->att[Black] & 0x70) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_OOO) == 0) return 0;
					if (occ & 0xE) return 0;
					if (Current->att[Black] & 0x1C) return 0;
				}
				return 1;
			}
		} else {
            if (IsCastling(move)) {
				if (u & 0x4000000000000000) {
                    if (((Current->castle_flags) & CanCastle_oo) == 0) return 0;
					if (occ & 0x6000000000000000) return 0;
					if (Current->att[White] & 0x7000000000000000) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_ooo) == 0) return 0;
					if (occ & 0x0E00000000000000) return 0;
					if (Current->att[White] & 0x1C00000000000000) return 0;
				}
				return 1;
			}
		}
        if (F(SArea[from] & u)) return 0;
	    if (Current->att[opp] & u) return 0;
		return 1;
	}
	piece = (piece >> 1) - 2;
	if (piece == 0) {
        if (u & NAtt[from]) return 1;
		else return 0;
	} else {
		if (piece <= 2) {
			if (BMask[from] & u) return 1;
		} else if (piece == 3) {
			if (RMask[from] & u) return 1;
		} else return 1;
		return 0;
	}
}



#ifdef TB // input TBPROBE code 
#ifdef TBPROBE
// The probing code currently expects a little-endian architecture (e.g. x86).

// Define DECOMP64 when compiling for a 64-bit platform.
// 32-bit is only supported for 5-piece tables, because tables are mmap()ed
// into memory.
#ifdef WINDOWS_X64
#define DECOMP64
#endif

#include <xstring>

extern int TBLargest;
void init(const std::string& path);
int probe_wdl(int *success);
int probe_dtz(int *success);

int probe_win(int * success) {
	if (Current->piece_nb > TBLargest) {
		*success = 0;
		return 0;
	}
	int score = 0;
	if (Current->piece_nb + (int)(Current - Data) >= 125) {
		*success = 0;
		return 0;
	}
	int value = probe_wdl(success);
	if (*success) {
#ifdef WINDOWS_X64
		InterlockedAdd64(&Smpi->tb_hits, (long long)1);
#else // WIN_X32
		Smpi->tb_hits++;
#endif // WINDOWS_X64
		if (Abs(value) <= 1) score = 0;
		else if (value < -1) score = -WdlValue + (int)(Current - Data) + (Current->piece_nb << 7);
		else if (value > 1) score = WdlValue - (int)(Current - Data) - (Current->piece_nb << 7);
		hash_low(0, score, 127);
		hash_high(score, 127);
	}
	return score;
}
int probe_distance(int * success) {
	if (Current->piece_nb > TBLargest) {
		*success = 0;
		return 0;
	}
	int score = 0;
	if (Current->piece_nb + (int)(Current - Data) >= 125) {
		*success = 0;
		return 0;
	}
	int value = probe_dtz(success);
	if (*success) {
#ifdef WINDOWS_X64
		InterlockedAdd64(&Smpi->tb_hits, (long long)1);
#else // WIN_X32
		Smpi->tb_hits++;
#endif // WINDOWS_X64
		if (value && Abs(value) <= 100) {
			if (Current->ply + Abs(value) > 100) return 0;
			if (Current->ply + Abs(value) == 100) return Sgn(value);
		}
		if (!value || Abs(value) > 100) score = 0;
		else {
			//if (value < 0) score = -DtzValue + 500 - value  ;// -(200 * F(Current->ply)); // Original
			//else if (value > 0) score = DtzValue - 500 - value; //+(200 * F(Current->ply)); // Original
			
			if (value < 0){
				score = (-MateScore + 500 - value - (200 * (Current->ply) + 2100)); // Revisar
				//fprintf(stdout, "info multipv 1 depth 0 score cp %d\n", score);
			}
			else if (value > 0){
				score = (MateScore - 500 - value + (200 * (Current->ply) - 2100)); // Revisar
				//	fprintf(stdout, "info multipv 1 depth 0 score cp %d\n", score);
			}
		
			
		}
	}
	return score;
}



#define __builtin_bswap64(x) _byteswap_uint64(x)
#define __builtin_bswap32(x) _byteswap_ulong(x)

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifndef _WIN32
#include <sys/mman.h>
#endif
#ifndef _WIN32
#include <pthread.h>
#define SEP_CHAR ':'
#define FD int
#define FD_ERR -1
#else
#include <windows.h>
#define SEP_CHAR ';'
#define FD HANDLE
#define FD_ERR INVALID_HANDLE_VALUE
#endif

#define WDLSUFFIX ".rtbw"
#define DTZSUFFIX ".rtbz"
#define WDLDIR "RTBWDIR"
#define DTZDIR "RTBZDIR"
#define TBPIECES 6

#define WDL_MAGIC 0x5d23e871
#define DTZ_MAGIC 0xa50c66d7

#define TBHASHBITS 11

typedef unsigned long long uint64;
typedef unsigned int uint32;
typedef unsigned char ubyte;
typedef unsigned short ushort;

struct TBHashEntry;

#ifdef DECOMP64
typedef uint64 base_t;
#else
typedef uint32 base_t;
#endif

struct PairsData {
	char *indextable;
	ushort *sizetable;
	ubyte *data;
	ushort *offset;
	ubyte *symlen;
	ubyte *sympat;
	int blocksize;
	int idxbits;
	int min_len;
	base_t base[1]; // C++ complains about base[]...
};

struct TBEntry {
	char *data;
	uint64 key;
	uint64 mapping;
	ubyte ready;
	ubyte num;
	ubyte symmetric;
	ubyte has_pawns;
};

struct TBEntry_piece {
	char *data;
	uint64 key;
	uint64 mapping;
	ubyte ready;
	ubyte num;
	ubyte symmetric;
	ubyte has_pawns;
	ubyte enc_type;
	struct PairsData *precomp[2];
	int factor[2][TBPIECES];
	ubyte pieces[2][TBPIECES];
	ubyte norm[2][TBPIECES];
};

struct TBEntry_pawn {
	char *data;
	uint64 key;
	uint64 mapping;
	ubyte ready;
	ubyte num;
	ubyte symmetric;
	ubyte has_pawns;
	ubyte pawns[2];
	struct {
		struct PairsData *precomp[2];
		int factor[2][TBPIECES];
		ubyte pieces[2][TBPIECES];
		ubyte norm[2][TBPIECES];
	} file[4];
};

struct DTZEntry_piece {
	char *data;
	uint64 key;
	uint64 mapping;
	ubyte ready;
	ubyte num;
	ubyte symmetric;
	ubyte has_pawns;
	ubyte enc_type;
	struct PairsData *precomp;
	int factor[TBPIECES];
	ubyte pieces[TBPIECES];
	ubyte norm[TBPIECES];
	ubyte flags; // accurate, mapped, side
	ushort map_idx[4];
	ubyte *map;
};

struct DTZEntry_pawn {
	char *data;
	uint64 key;
	uint64 mapping;
	ubyte ready;
	ubyte num;
	ubyte symmetric;
	ubyte has_pawns;
	ubyte pawns[2];
	struct {
		struct PairsData *precomp;
		int factor[TBPIECES];
		ubyte pieces[TBPIECES];
		ubyte norm[TBPIECES];
	} file[4];
	ubyte flags[4];
	ushort map_idx[4][4];
	ubyte *map;
};

struct TBHashEntry {
	uint64 key;
	struct TBEntry *ptr;
};

struct DTZTableEntry {
	uint64 key1;
	uint64 key2;
	struct TBEntry *entry;
};


#define TBMAX_PIECE 254
#define TBMAX_PAWN 256
#define HSHMAX 8

// for variants where kings can connect and/or captured
// #define CONNECTED_KINGS

#define Swap(a,b) {int tmp=a;a=b;b=tmp;}

#define TB_PAWN 1
#define TB_KNIGHT 2
#define TB_BISHOP 3
#define TB_ROOK 4
#define TB_QUEEN 5
#define TB_KING 6

#define TB_WPAWN TB_PAWN
#define TB_BPAWN (TB_PAWN | 8)

static volatile long TB_mutex = 0;

static bool initialized = false;
static int num_paths = 0;
static char *path_string = NULL;
static char **paths = NULL;

static int TBnum_piece, TBnum_pawn;
static struct TBEntry_piece TB_piece[TBMAX_PIECE];
static struct TBEntry_pawn TB_pawn[TBMAX_PAWN];

static struct TBHashEntry TB_hash[1 << TBHASHBITS][HSHMAX];

#define DTZ_ENTRIES 64

static struct DTZTableEntry DTZ_table[DTZ_ENTRIES];

static void init_indices(void);
static uint64 calc_key_from_pcs(int *pcs, int mirror);
static void free_wdl_entry(struct TBEntry *entry);
static void free_dtz_entry(struct TBEntry *entry);

static FD open_tb(const char *str, const char *suffix)
{
	int i;
	FD fd;
	char file[256];

	for (i = 0; i < num_paths; i++) {
		strcpy(file, paths[i]);
		strcat(file, "/");
		strcat(file, str);
		strcat(file, suffix);
#ifndef _WIN32
		fd = open(file, O_RDONLY);
#else
		fd = CreateFile(file, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
#endif
		if (fd != FD_ERR) return fd;
	}
	return FD_ERR;
}

static void close_tb(FD fd)
{
#ifndef _WIN32
	close(fd);
#else
	CloseHandle(fd);
#endif
}

static char *map_file(const char *name, const char *suffix, uint64 *mapping)
{
	FD fd = open_tb(name, suffix);
	if (fd == FD_ERR)
		return NULL;
#ifndef _WIN32
	struct stat statbuf;
	fstat(fd, &statbuf);
	*mapping = statbuf.st_size;
	char *data = (char *)mmap(NULL, statbuf.st_size, PROT_READ,
		MAP_SHARED, fd, 0);
	if (data == (char *)(-1)) {
		printf("Could not mmap() %s.\n", name);
		exit(1);
	}
#else
	DWORD size_low, size_high;
	size_low = GetFileSize(fd, &size_high);
	//  *size = ((uint64)size_high) << 32 | ((uint64)size_low);
	HANDLE map = CreateFileMapping(fd, NULL, PAGE_READONLY, size_high, size_low,
		NULL);
	if (map == NULL) {
		printf("CreateFileMapping() failed.\n");
		exit(1);
	}
	*mapping = (uint64)map;
	char *data = (char *)MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
	if (data == NULL) {
		printf("MapViewOfFile() failed, name = %s%s, error = %lu.\n", name, suffix, GetLastError());
		exit(1);
	}
#endif
	close_tb(fd);
	return data;
}

#ifndef _WIN32
static void unmap_file(char *data, uint64 size)
{
	if (!data) return;
	munmap(data, size);
}
#else
static void unmap_file(char *data, uint64 mapping)
{
	if (!data) return;
	UnmapViewOfFile(data);
	CloseHandle((HANDLE)mapping);
}
#endif

static void add_to_hash(struct TBEntry *ptr, uint64 key)
{
	int i, hshidx;

	hshidx = key >> (64 - TBHASHBITS);
	i = 0;
	while (i < HSHMAX && TB_hash[hshidx][i].ptr)
		i++;
	if (i == HSHMAX) {
		printf("HSHMAX too low!\n");
		exit(1);
	}
	else {
		TB_hash[hshidx][i].key = key;
		TB_hash[hshidx][i].ptr = ptr;
	}
}

static char pchr[] = { 'K', 'Q', 'R', 'B', 'N', 'P' };

static void init_tb(char *str)
{
	FD fd;
	struct TBEntry *entry;
	int i, j, pcs[16];
	uint64 key, key2;
	int color;
	char *s;

	fd = open_tb(str, WDLSUFFIX);
	if (fd == FD_ERR) return;
	close_tb(fd);

	for (i = 0; i < 16; i++)
		pcs[i] = 0;
	color = 0;
	for (s = str; *s; s++)
		switch (*s) {
		case 'P':
			pcs[TB_PAWN | color]++;
			break;
		case 'N':
			pcs[TB_KNIGHT | color]++;
			break;
		case 'B':
			pcs[TB_BISHOP | color]++;
			break;
		case 'R':
			pcs[TB_ROOK | color]++;
			break;
		case 'Q':
			pcs[TB_QUEEN | color]++;
			break;
		case 'K':
			pcs[TB_KING | color]++;
			break;
		case 'v':
			color = 0x08;
			break;
	}
	for (i = 0; i < 8; i++)
		if (pcs[i] != pcs[i + 8])
			break;
	key = calc_key_from_pcs(pcs, 0);
	key2 = calc_key_from_pcs(pcs, 1);
	if (pcs[TB_WPAWN] + pcs[TB_BPAWN] == 0) {
		if (TBnum_piece == TBMAX_PIECE) {
			printf("TBMAX_PIECE limit too low!\n");
			exit(1);
		}
		entry = (struct TBEntry *)&TB_piece[TBnum_piece++];
	}
	else {
		if (TBnum_pawn == TBMAX_PAWN) {
			printf("TBMAX_PAWN limit too low!\n");
			exit(1);
		}
		entry = (struct TBEntry *)&TB_pawn[TBnum_pawn++];
	}
	entry->key = key;
	entry->ready = 0;
	entry->num = 0;
	for (i = 0; i < 16; i++)
		entry->num += pcs[i];
	entry->symmetric = (key == key2);
	entry->has_pawns = (pcs[TB_WPAWN] + pcs[TB_BPAWN] > 0);
	if (entry->num > TBLargest)
		TBLargest = entry->num;

	if (entry->has_pawns) {
		struct TBEntry_pawn *ptr = (struct TBEntry_pawn *)entry;
		ptr->pawns[0] = pcs[TB_WPAWN];
		ptr->pawns[1] = pcs[TB_BPAWN];
		if (pcs[TB_BPAWN] > 0
			&& (pcs[TB_WPAWN] == 0 || pcs[TB_BPAWN] < pcs[TB_WPAWN])) {
			ptr->pawns[0] = pcs[TB_BPAWN];
			ptr->pawns[1] = pcs[TB_WPAWN];
		}
	}
	else {
		struct TBEntry_piece *ptr = (struct TBEntry_piece *)entry;
		for (i = 0, j = 0; i < 16; i++)
			if (pcs[i] == 1) j++;
		if (j >= 3) ptr->enc_type = 0;
		else if (j == 2) ptr->enc_type = 2;
		else { /* only for suicide */
			j = 16;
			for (i = 0; i < 16; i++) {
				if (pcs[i] < j && pcs[i] > 1) j = pcs[i];
				ptr->enc_type = 1 + j;
			}
		}
	}
	add_to_hash(entry, key);
	if (key2 != key) add_to_hash(entry, key2);
}

void init(const std::string& path)
{
	char str[16];
	int i, j, k, l;

	if (initialized) {
		free(path_string);
		free(paths);
		struct TBEntry *entry;
		for (i = 0; i < TBnum_piece; i++) {
			entry = (struct TBEntry *)&TB_piece[i];
			free_wdl_entry(entry);
		}
		for (i = 0; i < TBnum_pawn; i++) {
			entry = (struct TBEntry *)&TB_pawn[i];
			free_wdl_entry(entry);
		}
		for (i = 0; i < DTZ_ENTRIES; i++)
			if (DTZ_table[i].entry)
				free_dtz_entry(DTZ_table[i].entry);
	}
	else {
		init_indices();
		initialized = true;
	}

	const char *p = path.c_str();
	if (strlen(p) == 0) return;
	path_string = (char *)malloc(strlen(p) + 1);
	strcpy(path_string, p);
	num_paths = 0;
	for (i = 0;; i++) {
		if (path_string[i] != SEP_CHAR)
			num_paths++;
		while (path_string[i] && path_string[i] != SEP_CHAR)
			i++;
		if (!path_string[i]) break;
		path_string[i] = 0;
	}
	paths = (char **)malloc(num_paths * sizeof(char *));
	for (i = j = 0; i < num_paths; i++) {
		while (!path_string[j]) j++;
		paths[i] = &path_string[j];
		while (path_string[j]) j++;
	}

	TBnum_piece = TBnum_pawn = 0;
	TBLargest = HSHMAX;

	for (i = 0; i < (1 << TBHASHBITS); i++)
		for (j = 0; j < HSHMAX; j++) {
			TB_hash[i][j].key = 0ULL;
			TB_hash[i][j].ptr = NULL;
		}

	for (i = 0; i < DTZ_ENTRIES; i++)
		DTZ_table[i].entry = NULL;

	for (i = 1; i < 7; i++) {
		sprintf_s(str, "K%cvK", pchr[i]);
		init_tb(str);
	}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++) {
			sprintf_s(str, "K%cvK%c", pchr[i], pchr[j]);
			init_tb(str);
		}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++) {
			sprintf_s(str, "K%c%cvK", pchr[i], pchr[j]);
			init_tb(str);
		}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++)
			for (k = 1; k < 7; k++) {
				sprintf_s(str, "K%c%cvK%c", pchr[i], pchr[j], pchr[k]);
				init_tb(str);
			}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++)
			for (k = j; k < 7; k++) {
				sprintf_s(str, "K%c%c%cvK", pchr[i], pchr[j], pchr[k]);
				init_tb(str);
			}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++)
			for (k = i; k < 7; k++)
				for (l = (i == k) ? j : k; l < 7; l++) {
					sprintf_s(str, "K%c%cvK%c%c", pchr[i], pchr[j], pchr[k], pchr[l]);
					init_tb(str);
				}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++)
			for (k = j; k < 7; k++)
				for (l = 1; l < 7; l++) {
					sprintf_s(str, "K%c%c%cvK%c", pchr[i], pchr[j], pchr[k], pchr[l]);
					init_tb(str);
				}

	for (i = 1; i < 7; i++)
		for (j = i; j < 7; j++)
			for (k = j; k < 6; k++)
				for (l = k; l < 7; l++) {
					sprintf_s(str, "K%c%c%c%cvK", pchr[i], pchr[j], pchr[k], pchr[l]);
					init_tb(str);
				}

	printf("info string Found %d tablebases.\n", TBnum_piece + TBnum_pawn);
}

static const char offdiag[] = {
	0, -1, -1, -1, -1, -1, -1, -1,
	1, 0, -1, -1, -1, -1, -1, -1,
	1, 1, 0, -1, -1, -1, -1, -1,
	1, 1, 1, 0, -1, -1, -1, -1,
	1, 1, 1, 1, 0, -1, -1, -1,
	1, 1, 1, 1, 1, 0, -1, -1,
	1, 1, 1, 1, 1, 1, 0, -1,
	1, 1, 1, 1, 1, 1, 1, 0
};

static const ubyte triangle[] = {
	6, 0, 1, 2, 2, 1, 0, 6,
	0, 7, 3, 4, 4, 3, 7, 0,
	1, 3, 8, 5, 5, 8, 3, 1,
	2, 4, 5, 9, 9, 5, 4, 2,
	2, 4, 5, 9, 9, 5, 4, 2,
	1, 3, 8, 5, 5, 8, 3, 1,
	0, 7, 3, 4, 4, 3, 7, 0,
	6, 0, 1, 2, 2, 1, 0, 6
};

static const ubyte invtriangle[] = {
	1, 2, 3, 10, 11, 19, 0, 9, 18, 27
};

static const ubyte invdiag[] = {
	0, 9, 18, 27, 36, 45, 54, 63,
	7, 14, 21, 28, 35, 42, 49, 56
};

static const ubyte flipdiag[] = {
	0, 8, 16, 24, 32, 40, 48, 56,
	1, 9, 17, 25, 33, 41, 49, 57,
	2, 10, 18, 26, 34, 42, 50, 58,
	3, 11, 19, 27, 35, 43, 51, 59,
	4, 12, 20, 28, 36, 44, 52, 60,
	5, 13, 21, 29, 37, 45, 53, 61,
	6, 14, 22, 30, 38, 46, 54, 62,
	7, 15, 23, 31, 39, 47, 55, 63
};

static const ubyte lower[] = {
	28, 0, 1, 2, 3, 4, 5, 6,
	0, 29, 7, 8, 9, 10, 11, 12,
	1, 7, 30, 13, 14, 15, 16, 17,
	2, 8, 13, 31, 18, 19, 20, 21,
	3, 9, 14, 18, 32, 22, 23, 24,
	4, 10, 15, 19, 22, 33, 25, 26,
	5, 11, 16, 20, 23, 25, 34, 27,
	6, 12, 17, 21, 24, 26, 27, 35
};

static const ubyte diag[] = {
	0, 0, 0, 0, 0, 0, 0, 8,
	0, 1, 0, 0, 0, 0, 9, 0,
	0, 0, 2, 0, 0, 10, 0, 0,
	0, 0, 0, 3, 11, 0, 0, 0,
	0, 0, 0, 12, 4, 0, 0, 0,
	0, 0, 13, 0, 0, 5, 0, 0,
	0, 14, 0, 0, 0, 0, 6, 0,
	15, 0, 0, 0, 0, 0, 0, 7
};

static const ubyte flap[] = {
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 6, 12, 18, 18, 12, 6, 0,
	1, 7, 13, 19, 19, 13, 7, 1,
	2, 8, 14, 20, 20, 14, 8, 2,
	3, 9, 15, 21, 21, 15, 9, 3,
	4, 10, 16, 22, 22, 16, 10, 4,
	5, 11, 17, 23, 23, 17, 11, 5,
	0, 0, 0, 0, 0, 0, 0, 0
};

static const ubyte ptwist[] = {
	0, 0, 0, 0, 0, 0, 0, 0,
	47, 35, 23, 11, 10, 22, 34, 46,
	45, 33, 21, 9, 8, 20, 32, 44,
	43, 31, 19, 7, 6, 18, 30, 42,
	41, 29, 17, 5, 4, 16, 28, 40,
	39, 27, 15, 3, 2, 14, 26, 38,
	37, 25, 13, 1, 0, 12, 24, 36,
	0, 0, 0, 0, 0, 0, 0, 0
};

static const ubyte invflap[] = {
	8, 16, 24, 32, 40, 48,
	9, 17, 25, 33, 41, 49,
	10, 18, 26, 34, 42, 50,
	11, 19, 27, 35, 43, 51
};

static const ubyte invptwist[] = {
	52, 51, 44, 43, 36, 35, 28, 27, 20, 19, 12, 11,
	53, 50, 45, 42, 37, 34, 29, 26, 21, 18, 13, 10,
	54, 49, 46, 41, 38, 33, 30, 25, 22, 17, 14, 9,
	55, 48, 47, 40, 39, 32, 31, 24, 23, 16, 15, 8
};

static const ubyte file_to_file[] = {
	0, 1, 2, 3, 3, 2, 1, 0
};

static const short KK_idx[10][64] = {
	{ -1, -1, -1, 0, 1, 2, 3, 4,
	-1, -1, -1, 5, 6, 7, 8, 9,
	10, 11, 12, 13, 14, 15, 16, 17,
	18, 19, 20, 21, 22, 23, 24, 25,
	26, 27, 28, 29, 30, 31, 32, 33,
	34, 35, 36, 37, 38, 39, 40, 41,
	42, 43, 44, 45, 46, 47, 48, 49,
	50, 51, 52, 53, 54, 55, 56, 57 },
	{ 58, -1, -1, -1, 59, 60, 61, 62,
	63, -1, -1, -1, 64, 65, 66, 67,
	68, 69, 70, 71, 72, 73, 74, 75,
	76, 77, 78, 79, 80, 81, 82, 83,
	84, 85, 86, 87, 88, 89, 90, 91,
	92, 93, 94, 95, 96, 97, 98, 99,
	100, 101, 102, 103, 104, 105, 106, 107,
	108, 109, 110, 111, 112, 113, 114, 115 },
	{ 116, 117, -1, -1, -1, 118, 119, 120,
	121, 122, -1, -1, -1, 123, 124, 125,
	126, 127, 128, 129, 130, 131, 132, 133,
	134, 135, 136, 137, 138, 139, 140, 141,
	142, 143, 144, 145, 146, 147, 148, 149,
	150, 151, 152, 153, 154, 155, 156, 157,
	158, 159, 160, 161, 162, 163, 164, 165,
	166, 167, 168, 169, 170, 171, 172, 173 },
	{ 174, -1, -1, -1, 175, 176, 177, 178,
	179, -1, -1, -1, 180, 181, 182, 183,
	184, -1, -1, -1, 185, 186, 187, 188,
	189, 190, 191, 192, 193, 194, 195, 196,
	197, 198, 199, 200, 201, 202, 203, 204,
	205, 206, 207, 208, 209, 210, 211, 212,
	213, 214, 215, 216, 217, 218, 219, 220,
	221, 222, 223, 224, 225, 226, 227, 228 },
	{ 229, 230, -1, -1, -1, 231, 232, 233,
	234, 235, -1, -1, -1, 236, 237, 238,
	239, 240, -1, -1, -1, 241, 242, 243,
	244, 245, 246, 247, 248, 249, 250, 251,
	252, 253, 254, 255, 256, 257, 258, 259,
	260, 261, 262, 263, 264, 265, 266, 267,
	268, 269, 270, 271, 272, 273, 274, 275,
	276, 277, 278, 279, 280, 281, 282, 283 },
	{ 284, 285, 286, 287, 288, 289, 290, 291,
	292, 293, -1, -1, -1, 294, 295, 296,
	297, 298, -1, -1, -1, 299, 300, 301,
	302, 303, -1, -1, -1, 304, 305, 306,
	307, 308, 309, 310, 311, 312, 313, 314,
	315, 316, 317, 318, 319, 320, 321, 322,
	323, 324, 325, 326, 327, 328, 329, 330,
	331, 332, 333, 334, 335, 336, 337, 338 },
	{ -1, -1, 339, 340, 341, 342, 343, 344,
	-1, -1, 345, 346, 347, 348, 349, 350,
	-1, -1, 441, 351, 352, 353, 354, 355,
	-1, -1, -1, 442, 356, 357, 358, 359,
	-1, -1, -1, -1, 443, 360, 361, 362,
	-1, -1, -1, -1, -1, 444, 363, 364,
	-1, -1, -1, -1, -1, -1, 445, 365,
	-1, -1, -1, -1, -1, -1, -1, 446 },
	{ -1, -1, -1, 366, 367, 368, 369, 370,
	-1, -1, -1, 371, 372, 373, 374, 375,
	-1, -1, -1, 376, 377, 378, 379, 380,
	-1, -1, -1, 447, 381, 382, 383, 384,
	-1, -1, -1, -1, 448, 385, 386, 387,
	-1, -1, -1, -1, -1, 449, 388, 389,
	-1, -1, -1, -1, -1, -1, 450, 390,
	-1, -1, -1, -1, -1, -1, -1, 451 },
	{ 452, 391, 392, 393, 394, 395, 396, 397,
	-1, -1, -1, -1, 398, 399, 400, 401,
	-1, -1, -1, -1, 402, 403, 404, 405,
	-1, -1, -1, -1, 406, 407, 408, 409,
	-1, -1, -1, -1, 453, 410, 411, 412,
	-1, -1, -1, -1, -1, 454, 413, 414,
	-1, -1, -1, -1, -1, -1, 455, 415,
	-1, -1, -1, -1, -1, -1, -1, 456 },
	{ 457, 416, 417, 418, 419, 420, 421, 422,
	-1, 458, 423, 424, 425, 426, 427, 428,
	-1, -1, -1, -1, -1, 429, 430, 431,
	-1, -1, -1, -1, -1, 432, 433, 434,
	-1, -1, -1, -1, -1, 435, 436, 437,
	-1, -1, -1, -1, -1, 459, 438, 439,
	-1, -1, -1, -1, -1, -1, 460, 440,
	-1, -1, -1, -1, -1, -1, -1, 461 }
};


static int binomial[5][64];
static int pawnidx[5][24];
static int pfactor[5][4];


static void init_indices(void)
{
	int i, j, k;

	// binomial[k-1][n] = Bin(n, k)
	for (i = 0; i < 5; i++)
		for (j = 0; j < 64; j++) {
			int f = j;
			int l = 1;
			for (k = 1; k <= i; k++) {
				f *= (j - k);
				l *= (k + 1);
			}
			binomial[i][j] = f / l;
		}

	for (i = 0; i < 5; i++) {
		int s = 0;
		for (j = 0; j < 6; j++) {
			pawnidx[i][j] = s;
			s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
		}
		pfactor[i][0] = s;
		s = 0;
		for (; j < 12; j++) {
			pawnidx[i][j] = s;
			s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
		}
		pfactor[i][1] = s;
		s = 0;
		for (; j < 18; j++) {
			pawnidx[i][j] = s;
			s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
		}
		pfactor[i][2] = s;
		s = 0;
		for (; j < 24; j++) {
			pawnidx[i][j] = s;
			s += (i == 0) ? 1 : binomial[i - 1][ptwist[invflap[j]]];
		}
		pfactor[i][3] = s;
	}

}


static uint64 encode_piece(struct TBEntry_piece *ptr, ubyte *norm, int *pos, int *factor)
{
	uint64 idx;
	int i, j, k, m, l, p;
	int n = ptr->num;

	if (pos[0] & 0x04) {
		for (i = 0; i < n; i++)
			pos[i] ^= 0x07;
	}
	if (pos[0] & 0x20) {
		for (i = 0; i < n; i++)
			pos[i] ^= 0x38;
	}

	for (i = 0; i < n; i++)
		if (offdiag[pos[i]]) break;
	if (i < (ptr->enc_type == 0 ? 3 : 2) && offdiag[pos[i]] > 0)
		for (i = 0; i < n; i++)
			pos[i] = flipdiag[pos[i]];

	switch (ptr->enc_type) {

	case 0: /* 111 */
		i = (pos[1] > pos[0]);
		j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

		if (offdiag[pos[0]])
			idx = triangle[pos[0]] * 63 * 62 + (pos[1] - i) * 62 + (pos[2] - j);
		else if (offdiag[pos[1]])
			idx = 6 * 63 * 62 + diag[pos[0]] * 28 * 62 + lower[pos[1]] * 62 + pos[2] - j;
		else if (offdiag[pos[2]])
			idx = 6 * 63 * 62 + 4 * 28 * 62 + (diag[pos[0]]) * 7 * 28 + (diag[pos[1]] - i) * 28 + lower[pos[2]];
		else
			idx = 6 * 63 * 62 + 4 * 28 * 62 + 4 * 7 * 28 + (diag[pos[0]] * 7 * 6) + (diag[pos[1]] - i) * 6 + (diag[pos[2]] - j);
		i = 3;
		break;

	case 1: /* K3 */
		j = (pos[2] > pos[0]) + (pos[2] > pos[1]);

		idx = KK_idx[triangle[pos[0]]][pos[1]];
		if (idx < 441)
			idx = idx + 441 * (pos[2] - j);
		else {
			idx = 441 * 62 + (idx - 441) + 21 * lower[pos[2]];
			if (!offdiag[pos[2]])
				idx -= j * 21;
		}
		i = 3;
		break;

	default: /* K2 */
		idx = KK_idx[triangle[pos[0]]][pos[1]];
		i = 2;
		break;
	}
	idx *= factor[0];

	for (; i < n;) {
		int t = norm[i];
		for (j = i; j < i + t; j++)
			for (k = j + 1; k < i + t; k++)
				if (pos[j] > pos[k]) Swap(pos[j], pos[k]);
		int s = 0;
		for (m = i; m < i + t; m++) {
			p = pos[m];
			for (l = 0, j = 0; l < i; l++)
				j += (p > pos[l]);
			s += binomial[m - i][p - j];
		}
		idx += ((uint64)s) * ((uint64)factor[i]);
		i += t;
	}

	return idx;
}


// determine file of leftmost pawn and sort pawns
static int pawn_file(struct TBEntry_pawn *ptr, int *pos)
{
	int i;

	for (i = 1; i < ptr->pawns[0]; i++)
		if (flap[pos[0]] > flap[pos[i]])
			Swap(pos[0], pos[i]);

	return file_to_file[pos[0] & 0x07];
}

static uint64 encode_pawn(struct TBEntry_pawn *ptr, ubyte *norm, int *pos, int *factor)
{
	uint64 idx;
	int i, j, k, m, s, t;
	int n = ptr->num;

	if (pos[0] & 0x04)
		for (i = 0; i < n; i++)
			pos[i] ^= 0x07;

	for (i = 1; i < ptr->pawns[0]; i++)
		for (j = i + 1; j < ptr->pawns[0]; j++)
			if (ptwist[pos[i]] < ptwist[pos[j]])
				Swap(pos[i], pos[j]);

	t = ptr->pawns[0] - 1;
	idx = pawnidx[t][flap[pos[0]]];
	for (i = t; i > 0; i--)
		idx += binomial[t - i][ptwist[pos[i]]];
	idx *= factor[0];

	// remaining pawns
	i = ptr->pawns[0];
	t = i + ptr->pawns[1];
	if (t > i) {
		for (j = i; j < t; j++)
			for (k = j + 1; k < t; k++)
				if (pos[j] > pos[k]) Swap(pos[j], pos[k]);
		s = 0;
		for (m = i; m < t; m++) {
			int p = pos[m];
			for (k = 0, j = 0; k < i; k++)
				j += (p > pos[k]);
			s += binomial[m - i][p - j - 8];
		}
		idx += ((uint64)s) * ((uint64)factor[i]);
		i = t;
	}

	for (; i < n;) {
		t = norm[i];
		for (j = i; j < i + t; j++)
			for (k = j + 1; k < i + t; k++)
				if (pos[j] > pos[k]) Swap(pos[j], pos[k]);
		s = 0;
		for (m = i; m < i + t; m++) {
			int p = pos[m];
			for (k = 0, j = 0; k < i; k++)
				j += (p > pos[k]);
			s += binomial[m - i][p - j];
		}
		idx += ((uint64)s) * ((uint64)factor[i]);
		i += t;
	}

	return idx;
}

static ubyte decompress_pairs(struct PairsData *d, uint64 index);

// place k like pieces on n squares
static int subfactor(int k, int n)
{
	int i, f, l;

	f = n;
	l = 1;
	for (i = 1; i < k; i++) {
		f *= n - i;
		l *= i + 1;
	}

	return f / l;
}

static uint64 calc_factors_piece(int *factor, int num, int order, ubyte *norm, ubyte enc_type)
{
	int i, k, n;
	uint64 f;

	static int pivfac[] = { 31332, 28056, 462 };


	n = 64 - norm[0];

	f = 1;
	for (i = norm[0], k = 0; i < num || k == order; k++) {
		if (k == order) {
			factor[0] = f;

			f *= pivfac[enc_type];

		}
		else {
			factor[i] = f;
			f *= subfactor(norm[i], n);
			n -= norm[i];
			i += norm[i];
		}
	}

	return f;
}

static uint64 calc_factors_pawn(int *factor, int num, int order, int order2, ubyte *norm, int file)
{
	int i, k, n;
	uint64 f;

	i = norm[0];
	if (order2 < 0x0f) i += norm[i];
	n = 64 - i;

	f = 1;
	for (k = 0; i < num || k == order || k == order2; k++) {
		if (k == order) {
			factor[0] = f;
			f *= pfactor[norm[0] - 1][file];
		}
		else if (k == order2) {
			factor[norm[0]] = f;
			f *= subfactor(norm[norm[0]], 48 - norm[0]);
		}
		else {
			factor[i] = f;
			f *= subfactor(norm[i], n);
			n -= norm[i];
			i += norm[i];
		}
	}

	return f;
}

static void set_norm_piece(struct TBEntry_piece *ptr, ubyte *norm, ubyte *pieces)
{
	int i, j;

	for (i = 0; i < ptr->num; i++)
		norm[i] = 0;

	switch (ptr->enc_type) {
	case 0:
		norm[0] = 3;
		break;
	case 2:
		norm[0] = 2;
		break;
	default:
		norm[0] = ptr->enc_type - 1;
		break;
	}

	for (i = norm[0]; i < ptr->num; i += norm[i])
		for (j = i; j < ptr->num && pieces[j] == pieces[i]; j++)
			norm[i]++;
}

static void set_norm_pawn(struct TBEntry_pawn *ptr, ubyte *norm, ubyte *pieces)
{
	int i, j;

	for (i = 0; i < ptr->num; i++)
		norm[i] = 0;

	norm[0] = ptr->pawns[0];
	if (ptr->pawns[1]) norm[ptr->pawns[0]] = ptr->pawns[1];

	for (i = ptr->pawns[0] + ptr->pawns[1]; i < ptr->num; i += norm[i])
		for (j = i; j < ptr->num && pieces[j] == pieces[i]; j++)
			norm[i]++;
}

static void setup_pieces_piece(struct TBEntry_piece *ptr, unsigned char *data, uint64 *tb_size)
{
	int i;
	int order;

	for (i = 0; i < ptr->num; i++)
		ptr->pieces[0][i] = data[i + 1] & 0x0f;
	order = data[0] & 0x0f;
	set_norm_piece(ptr, ptr->norm[0], ptr->pieces[0]);
	tb_size[0] = calc_factors_piece(ptr->factor[0], ptr->num, order, ptr->norm[0], ptr->enc_type);

	for (i = 0; i < ptr->num; i++)
		ptr->pieces[1][i] = data[i + 1] >> 4;
	order = data[0] >> 4;
	set_norm_piece(ptr, ptr->norm[1], ptr->pieces[1]);
	tb_size[1] = calc_factors_piece(ptr->factor[1], ptr->num, order, ptr->norm[1], ptr->enc_type);
}

static void setup_pieces_piece_dtz(struct DTZEntry_piece *ptr, unsigned char *data, uint64 *tb_size)
{
	int i;
	int order;

	for (i = 0; i < ptr->num; i++)
		ptr->pieces[i] = data[i + 1] & 0x0f;
	order = data[0] & 0x0f;
	set_norm_piece((struct TBEntry_piece *)ptr, ptr->norm, ptr->pieces);
	tb_size[0] = calc_factors_piece(ptr->factor, ptr->num, order, ptr->norm, ptr->enc_type);
}

static void setup_pieces_pawn(struct TBEntry_pawn *ptr, unsigned char *data, uint64 *tb_size, int f)
{
	int i, j;
	int order, order2;

	j = 1 + (ptr->pawns[1] > 0);
	order = data[0] & 0x0f;
	order2 = ptr->pawns[1] ? (data[1] & 0x0f) : 0x0f;
	for (i = 0; i < ptr->num; i++)
		ptr->file[f].pieces[0][i] = data[i + j] & 0x0f;
	set_norm_pawn(ptr, ptr->file[f].norm[0], ptr->file[f].pieces[0]);
	tb_size[0] = calc_factors_pawn(ptr->file[f].factor[0], ptr->num, order, order2, ptr->file[f].norm[0], f);

	order = data[0] >> 4;
	order2 = ptr->pawns[1] ? (data[1] >> 4) : 0x0f;
	for (i = 0; i < ptr->num; i++)
		ptr->file[f].pieces[1][i] = data[i + j] >> 4;
	set_norm_pawn(ptr, ptr->file[f].norm[1], ptr->file[f].pieces[1]);
	tb_size[1] = calc_factors_pawn(ptr->file[f].factor[1], ptr->num, order, order2, ptr->file[f].norm[1], f);
}

static void setup_pieces_pawn_dtz(struct DTZEntry_pawn *ptr, unsigned char *data, uint64 *tb_size, int f)
{
	int i, j;
	int order, order2;

	j = 1 + (ptr->pawns[1] > 0);
	order = data[0] & 0x0f;
	order2 = ptr->pawns[1] ? (data[1] & 0x0f) : 0x0f;
	for (i = 0; i < ptr->num; i++)
		ptr->file[f].pieces[i] = data[i + j] & 0x0f;
	set_norm_pawn((struct TBEntry_pawn *)ptr, ptr->file[f].norm, ptr->file[f].pieces);
	tb_size[0] = calc_factors_pawn(ptr->file[f].factor, ptr->num, order, order2, ptr->file[f].norm, f);
}

static void calc_symlen(struct PairsData *d, int s, char *tmp)
{
	int s1, s2;

	int w = *(int *)(d->sympat + 3 * s);
	s2 = (w >> 12) & 0x0fff;
	if (s2 == 0x0fff)
		d->symlen[s] = 0;
	else {
		s1 = w & 0x0fff;
		if (!tmp[s1]) calc_symlen(d, s1, tmp);
		if (!tmp[s2]) calc_symlen(d, s2, tmp);
		d->symlen[s] = d->symlen[s1] + d->symlen[s2] + 1;
	}
	tmp[s] = 1;
}

static struct PairsData *setup_pairs(unsigned char *data, uint64 tb_size, uint64 *size, unsigned char **next, ubyte *flags, int wdl)
{
	struct PairsData *d;
	int i;

	*flags = data[0];
	if (data[0] & 0x80) {
		d = (struct PairsData *)malloc(sizeof(struct PairsData));
		d->idxbits = 0;
		if (wdl)
			d->min_len = data[1];
		else
			d->min_len = 0;
		*next = data + 2;
		size[0] = size[1] = size[2] = 0;
		return d;
	}

	int blocksize = data[1];
	int idxbits = data[2];
	int real_num_blocks = *(uint32 *)(&data[4]);
	int num_blocks = real_num_blocks + *(ubyte *)(&data[3]);
	int max_len = data[8];
	int min_len = data[9];
	int h = max_len - min_len + 1;
	int num_syms = *(ushort *)(&data[10 + 2 * h]);
	d = (struct PairsData *)malloc(sizeof(struct PairsData) + (h - 1) * sizeof(base_t) + num_syms);
	d->blocksize = blocksize;
	d->idxbits = idxbits;
	d->offset = (ushort *)(&data[10]);
	d->symlen = ((ubyte *)d) + sizeof(struct PairsData) + (h - 1) * sizeof(base_t);
	d->sympat = &data[12 + 2 * h];
	d->min_len = min_len;
	*next = &data[12 + 2 * h + 3 * num_syms + (num_syms & 1)];

	int num_indices = (tb_size + (1ULL << idxbits) - 1) >> idxbits;
	size[0] = 6ULL * num_indices;
	size[1] = 2ULL * num_blocks;
	size[2] = (1ULL << blocksize) * real_num_blocks;

	// char tmp[num_syms];
	char tmp[4096];
	for (i = 0; i < num_syms; i++)
		tmp[i] = 0;
	for (i = 0; i < num_syms; i++)
		if (!tmp[i])
			calc_symlen(d, i, tmp);

	d->base[h - 1] = 0;
	for (i = h - 2; i >= 0; i--)
		d->base[i] = (d->base[i + 1] + d->offset[i] - d->offset[i + 1]) / 2;
#ifdef DECOMP64
	for (i = 0; i < h; i++)
		d->base[i] <<= 64 - (min_len + i);
#else
	for (i = 0; i < h; i++)
		d->base[i] <<= 32 - (min_len + i);
#endif

	d->offset -= d->min_len;

	return d;
}

static int init_table_wdl(struct TBEntry *entry, char *str)
{
	ubyte *next;
	int f, s;
	uint64 tb_size[8];
	uint64 size[8 * 3];
	ubyte flags;

	// first mmap the table into memory

	entry->data = map_file(str, WDLSUFFIX, &entry->mapping);
	if (!entry->data) {
		printf("Could not find %s" WDLSUFFIX, str);
		return 0;
	}

	ubyte *data = (ubyte *)entry->data;
	if (((uint32 *)data)[0] != WDL_MAGIC) {
		printf("Corrupted table.\n");
		unmap_file(entry->data, entry->mapping);
		entry->data = 0;
		return 0;
	}

	int split = data[4] & 0x01;
	int files = data[4] & 0x02 ? 4 : 1;

	data += 5;

	if (!entry->has_pawns) {
		struct TBEntry_piece *ptr = (struct TBEntry_piece *)entry;
		setup_pieces_piece(ptr, data, &tb_size[0]);
		data += ptr->num + 1;
		data += ((uintptr_t)data) & 0x01;

		ptr->precomp[0] = setup_pairs(data, tb_size[0], &size[0], &next, &flags, 1);
		data = next;
		if (split) {
			ptr->precomp[1] = setup_pairs(data, tb_size[1], &size[3], &next, &flags, 1);
			data = next;
		}
		else
			ptr->precomp[1] = NULL;

		ptr->precomp[0]->indextable = (char *)data;
		data += size[0];
		if (split) {
			ptr->precomp[1]->indextable = (char *)data;
			data += size[3];
		}

		ptr->precomp[0]->sizetable = (ushort *)data;
		data += size[1];
		if (split) {
			ptr->precomp[1]->sizetable = (ushort *)data;
			data += size[4];
		}

		data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
		ptr->precomp[0]->data = data;
		data += size[2];
		if (split) {
			data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
			ptr->precomp[1]->data = data;
		}
	}
	else {
		struct TBEntry_pawn *ptr = (struct TBEntry_pawn *)entry;
		s = 1 + (ptr->pawns[1] > 0);
		for (f = 0; f < 4; f++) {
			setup_pieces_pawn((struct TBEntry_pawn *)ptr, data, &tb_size[2 * f], f);
			data += ptr->num + s;
		}
		data += ((uintptr_t)data) & 0x01;

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp[0] = setup_pairs(data, tb_size[2 * f], &size[6 * f], &next, &flags, 1);
			data = next;
			if (split) {
				ptr->file[f].precomp[1] = setup_pairs(data, tb_size[2 * f + 1], &size[6 * f + 3], &next, &flags, 1);
				data = next;
			}
			else
				ptr->file[f].precomp[1] = NULL;
		}

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp[0]->indextable = (char *)data;
			data += size[6 * f];
			if (split) {
				ptr->file[f].precomp[1]->indextable = (char *)data;
				data += size[6 * f + 3];
			}
		}

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp[0]->sizetable = (ushort *)data;
			data += size[6 * f + 1];
			if (split) {
				ptr->file[f].precomp[1]->sizetable = (ushort *)data;
				data += size[6 * f + 4];
			}
		}

		for (f = 0; f < files; f++) {
			data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
			ptr->file[f].precomp[0]->data = data;
			data += size[6 * f + 2];
			if (split) {
				data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
				ptr->file[f].precomp[1]->data = data;
				data += size[6 * f + 5];
			}
		}
	}

	return 1;
}

static int init_table_dtz(struct TBEntry *entry)
{
	ubyte *data = (ubyte *)entry->data;
	ubyte *next;
	int f, s;
	uint64 tb_size[4];
	uint64 size[4 * 3];

	if (!data)
		return 0;

	if (((uint32 *)data)[0] != DTZ_MAGIC) {
		printf("Corrupted table.\n");
		return 0;
	}

	int files = data[4] & 0x02 ? 4 : 1;

	data += 5;

	if (!entry->has_pawns) {
		struct DTZEntry_piece *ptr = (struct DTZEntry_piece *)entry;
		setup_pieces_piece_dtz(ptr, data, &tb_size[0]);
		data += ptr->num + 1;
		data += ((uintptr_t)data) & 0x01;

		ptr->precomp = setup_pairs(data, tb_size[0], &size[0], &next, &(ptr->flags), 0);
		data = next;

		ptr->map = data;
		if (ptr->flags & 2) {
			int i;
			for (i = 0; i < 4; i++) {
				ptr->map_idx[i] = (data + 1 - ptr->map);
				data += 1 + data[0];
			}
			data += ((uintptr_t)data) & 0x01;
		}

		ptr->precomp->indextable = (char *)data;
		data += size[0];

		ptr->precomp->sizetable = (ushort *)data;
		data += size[1];

		data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
		ptr->precomp->data = data;
		data += size[2];
	}
	else {
		struct DTZEntry_pawn *ptr = (struct DTZEntry_pawn *)entry;
		s = 1 + (ptr->pawns[1] > 0);
		for (f = 0; f < 4; f++) {
			setup_pieces_pawn_dtz(ptr, data, &tb_size[f], f);
			data += ptr->num + s;
		}
		data += ((uintptr_t)data) & 0x01;

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp = setup_pairs(data, tb_size[f], &size[3 * f], &next, &(ptr->flags[f]), 0);
			data = next;
		}

		ptr->map = data;
		for (f = 0; f < files; f++) {
			if (ptr->flags[f] & 2) {
				int i;
				for (i = 0; i < 4; i++) {
					ptr->map_idx[f][i] = (data + 1 - ptr->map);
					data += 1 + data[0];
				}
			}
		}
		data += ((uintptr_t)data) & 0x01;

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp->indextable = (char *)data;
			data += size[3 * f];
		}

		for (f = 0; f < files; f++) {
			ptr->file[f].precomp->sizetable = (ushort *)data;
			data += size[3 * f + 1];
		}

		for (f = 0; f < files; f++) {
			data = (ubyte *)((((uintptr_t)data) + 0x3f) & ~0x3f);
			ptr->file[f].precomp->data = data;
			data += size[3 * f + 2];
		}
	}

	return 1;
}

static ubyte decompress_pairs(struct PairsData *d, uint64 idx)
{
	if (!d->idxbits)
		return d->min_len;

	uint32 mainidx = idx >> d->idxbits;
	int litidx = (idx & ((1 << d->idxbits) - 1)) - (1 << (d->idxbits - 1));
	uint32 block = *(uint32 *)(d->indextable + 6 * mainidx);
	litidx += *(ushort *)(d->indextable + 6 * mainidx + 4);
	if (litidx < 0) {
		do {
			litidx += d->sizetable[--block] + 1;
		} while (litidx < 0);
	}
	else {
		while (litidx > d->sizetable[block])
			litidx -= d->sizetable[block++] + 1;
	}

	uint32 *ptr = (uint32 *)(d->data + (block << d->blocksize));

	int m = d->min_len;
	ushort *offset = d->offset;
	base_t *base = d->base - m;
	ubyte *symlen = d->symlen;
	int sym, bitcnt;

#ifdef DECOMP64
	uint64 code = __builtin_bswap64(*((uint64 *)ptr));
	ptr += 2;
	bitcnt = 0; // number of "empty bits" in code
	for (;;) {
		int l = m;
		while (code < base[l]) l++;
		sym = offset[l] + ((code - base[l]) >> (64 - l));
		if (litidx < (int)symlen[sym] + 1) break;
		litidx -= (int)symlen[sym] + 1;
		code <<= l;
		bitcnt += l;
		if (bitcnt >= 32) {
			bitcnt -= 32;
			code |= ((uint64)(__builtin_bswap32(*ptr++))) << bitcnt;
		}
	}
#else // 32 bits
	uint32 next = 0;
	uint32 code = __builtin_bswap32(*ptr++);
	bitcnt = 0; // number of bits in next
	for (;;) {
		int l = m;
		while (code < base[l]) l++;
		sym = offset[l] + ((code - base[l]) >> (32 - l));
		if (litidx < (int)symlen[sym] + 1) break;
		litidx -= (int)symlen[sym] + 1;
		code <<= l;
		if (bitcnt < l) {
			if (bitcnt) {
				code |= (next >> (32 - l));
				l -= bitcnt;
			}
			next = __builtin_bswap32(*ptr++);
			bitcnt = 32;
		}
		code |= (next >> (32 - l));
		next <<= l;
		bitcnt -= l;
	}
#endif

	ubyte *sympat = d->sympat;
	while (symlen[sym] != 0) {
		int w = *(int *)(sympat + 3 * sym);
		int s1 = w & 0x0fff;
		if (litidx < (int)symlen[s1] + 1)
			sym = s1;
		else {
			litidx -= (int)symlen[s1] + 1;
			sym = (w >> 12) & 0x0fff;
		}
	}

	return *(sympat + 3 * sym);
}

void load_dtz_table(char *str, uint64 key1, uint64 key2)
{
	int i;
	struct TBEntry *ptr, *ptr3;
	struct TBHashEntry *ptr2;

	DTZ_table[0].key1 = key1;
	DTZ_table[0].key2 = key2;
	DTZ_table[0].entry = NULL;

	// find corresponding WDL entry
	ptr2 = TB_hash[key1 >> (64 - TBHASHBITS)];
	for (i = 0; i < HSHMAX; i++)
		if (ptr2[i].key == key1) break;
	if (i == HSHMAX) return;
	ptr = ptr2[i].ptr;

	ptr3 = (struct TBEntry *)malloc(ptr->has_pawns
		? sizeof(struct DTZEntry_pawn)
		: sizeof(struct DTZEntry_piece));

	ptr3->data = map_file(str, DTZSUFFIX, &ptr3->mapping);
	ptr3->key = ptr->key;
	ptr3->num = ptr->num;
	ptr3->symmetric = ptr->symmetric;
	ptr3->has_pawns = ptr->has_pawns;
	if (ptr3->has_pawns) {
		struct DTZEntry_pawn *entry = (struct DTZEntry_pawn *)ptr3;
		entry->pawns[0] = ((struct TBEntry_pawn *)ptr)->pawns[0];
		entry->pawns[1] = ((struct TBEntry_pawn *)ptr)->pawns[1];
	}
	else {
		struct DTZEntry_piece *entry = (struct DTZEntry_piece *)ptr3;
		entry->enc_type = ((struct TBEntry_piece *)ptr)->enc_type;
	}
	if (!init_table_dtz(ptr3))
		free(ptr3);
	else
		DTZ_table[0].entry = ptr3;
}

static void free_wdl_entry(struct TBEntry *entry)
{
	unmap_file(entry->data, entry->mapping);
	if (!entry->has_pawns) {
		struct TBEntry_piece *ptr = (struct TBEntry_piece *)entry;
		free(ptr->precomp[0]);
		if (ptr->precomp[1])
			free(ptr->precomp[1]);
	}
	else {
		struct TBEntry_pawn *ptr = (struct TBEntry_pawn *)entry;
		int f;
		for (f = 0; f < 4; f++) {
			free(ptr->file[f].precomp[0]);
			if (ptr->file[f].precomp[1])
				free(ptr->file[f].precomp[1]);
		}
	}
}

static void free_dtz_entry(struct TBEntry *entry)
{
	unmap_file(entry->data, entry->mapping);
	if (!entry->has_pawns) {
		struct DTZEntry_piece *ptr = (struct DTZEntry_piece *)entry;
		free(ptr->precomp);
	}
	else {
		struct DTZEntry_pawn *ptr = (struct DTZEntry_pawn *)entry;
		int f;
		for (f = 0; f < 4; f++)
			free(ptr->file[f].precomp);
	}
	free(entry);
}

static int wdl_to_map[5] = { 1, 3, 0, 2, 0 };
static ubyte pa_flags[5] = { 8, 0, 0, 0, 4 };



#endif 

#define psq(color, piece_type, sq) PieceKey[((piece_type) << 1) | color][sq] 
#define WHITE White
#define BLACK Black
#define KING 6
#define QUEEN 5
#define PAWN 1
#define Bitboard uint64

const int piece_from_pt[7] = { 0, 2, 4, 6, 10, 12, 14 };
const int pt_from_piece[16] = { 0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6 };

__forceinline uint64 bb_color_pt(int color, int pt) {
	if (pt == 3) return Bishop(color);
	else return BB(piece_from_pt[pt] | color);
}

__forceinline int pop_lsb(uint64 * x) {
	int sq = lsb(*x);
	*x ^= Bit(sq);
	return sq;
}

int TBLargest = 0;

void compute_pos_info() {
	if (Current->eval_key == Current->key) return;
	evaluate(); return;

	Current->patt[White] = ShiftW(White, Pawn(White)) | ShiftE(White, Pawn(White));
	Current->patt[Black] = ShiftW(Black, Pawn(Black)) | ShiftE(Black, Pawn(Black));
	int king_w = lsb(King(White)), king_b = lsb(King(Black));
	Current->att[White] = Current->patt[White] | SArea[king_w];
	Current->att[Black] = Current->patt[Black] | SArea[king_b];
	Current->xray[White] = Current->xray[Black] = 0;
	uint64 occ = PieceAll;
#define me White
	for (uint64 u = Knight(me); u; Cut(u)) Current->att[me] |= NAtt[lsb(u)];
	for (uint64 u = Bishop(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = BishopAttacks(sq, occ);
		Current->att[me] |= att;
		if (BMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
	for (uint64 u = Rook(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = RookAttacks(sq, occ);
		Current->att[me] |= att;
		if (RMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
	for (uint64 u = Queen(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = QueenAttacks(sq, occ);
		Current->att[me] |= att;
		if (QMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
#undef me
#define me Black
	for (uint64 u = Knight(me); u; Cut(u)) Current->att[me] |= NAtt[lsb(u)];
	for (uint64 u = Bishop(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = BishopAttacks(sq, occ);
		Current->att[me] |= att;
		if (BMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
	for (uint64 u = Rook(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = RookAttacks(sq, occ);
		Current->att[me] |= att;
		if (RMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
	for (uint64 u = Queen(me); u; Cut(u)) {
		uint64 sq = lsb(u);
		uint64 att = QueenAttacks(sq, occ);
		Current->att[me] |= att;
		if (QMask[sq] & King(opp)) if (uint64 v = (Between[VarC(king, opp)][sq] & occ)) if (Single(v)) Current->xray[me] |= v;
	}
#undef me
}

// Given a position with 6 or fewer pieces, produce a text string
// of the form KQPvKRP, where "KQP" represents the white pieces if
// mirror == 0 and the black pieces if mirror == 1.
static void prt_str(char *str, int mirror)
{
	int color, pt, i;

	color = !mirror ? WHITE : BLACK;
	for (pt = KING; pt >= PAWN; --pt)
		for (i = popcnt(bb_color_pt(color, pt)); i > 0; i--)
			*str++ = pchr[6 - pt];
	*str++ = 'v';
	color ^= 1;
	for (pt = KING; pt >= PAWN; --pt)
		for (i = popcnt(bb_color_pt(color, pt)); i > 0; i--)
			*str++ = pchr[6 - pt];
	*str++ = 0;
}

// Given a position, produce a 64-bit material signature key.
// If the ENGINE supports such a key, it should equal the ENGINE's key.
static uint64 calc_key(int mirror)
{
	int color, pt, i;
	uint64 key = 0;

	color = !mirror ? WHITE : BLACK;
	for (pt = PAWN; pt <= KING; ++pt)
		for (i = popcnt(bb_color_pt(color, pt)); i > 0; i--)
			key ^= psq(WHITE, pt, i - 1);
	color ^= 1;
	for (pt = PAWN; pt <= KING; ++pt)
		for (i = popcnt(bb_color_pt(color, pt)); i > 0; i--)
			key ^= psq(BLACK, pt, i - 1);

	return key;
}

// Produce a 64-bit material key corresponding to the material combination
// defined by pcs[16], where pcs[1], ..., pcs[6] is the number of white
// pawns, ..., kings and pcs[9], ..., pcs[14] is the number of black
// pawns, ..., kings.
static uint64 calc_key_from_pcs(int *pcs, int mirror)
{
	int color, pt, i;
	uint64 key = 0;

	color = !mirror ? 0 : 8;
	for (pt = PAWN; pt <= KING; ++pt)
		for (i = 0; i < pcs[color + pt]; i++)
			key ^= psq(WHITE, pt, i);
	color ^= 8;
	for (pt = PAWN; pt <= KING; ++pt)
		for (i = 0; i < pcs[color + pt]; i++)
			key ^= psq(BLACK, pt, i);

	return key;
}

// probe_wdl_table and probe_dtz_table require similar adaptations.
static int probe_wdl_table(int *success)
{
	struct TBEntry *ptr;
	struct TBHashEntry *ptr2;
	uint64 idx;
	uint64 key;
	int i;
	ubyte res;
	int p[TBPIECES];

	// Obtain the position's material signature key.
	key = calc_key(0);

	// Test for KvK.
	if (key == (psq(WHITE, KING, 0) ^ psq(BLACK, KING, 0)))
		return 0;

	ptr2 = TB_hash[key >> (64 - TBHASHBITS)];
	for (i = 0; i < HSHMAX; i++)
		if (ptr2[i].key == key) break;
	if (i == HSHMAX) {
		*success = 0;
		return 0;
	}

	ptr = ptr2[i].ptr;
	if (!ptr->ready) {
		LOCK(TB_mutex);
		if (!ptr->ready) {
			char str[16];
			prt_str(str, ptr->key != key);
			if (!init_table_wdl(ptr, str)) {
				ptr2[i].key = 0ULL;
				*success = 0;
				UNLOCK(TB_mutex);
				return 0;
			}
			// Memory barrier to ensure ptr->ready = 1 is not reordered.
			ptr->ready = 1;
		}
		UNLOCK(TB_mutex);
	}

	int bside, mirror, cmirror;
	if (!ptr->symmetric) {
		if (key != ptr->key) {
			cmirror = 8;
			mirror = 0x38;
			bside = (Current->turn == WHITE);
		}
		else {
			cmirror = mirror = 0;
			bside = !(Current->turn == WHITE);
		}
	}
	else {
		cmirror = Current->turn == WHITE ? 0 : 8;
		mirror = Current->turn == WHITE ? 0 : 0x38;
		bside = 0;
	}

	// p[i] is to contain the square 0-63 (A1-H8) for a piece of type
	// pc[i] ^ cmirror, where 1 = white pawn, ..., 14 = black king.
	// Pieces of the same type are guaranteed to be consecutive.
	if (!ptr->has_pawns) {
		struct TBEntry_piece *entry = (struct TBEntry_piece *)ptr;
		ubyte *pc = entry->pieces[bside];
		for (i = 0; i < entry->num;) {
			Bitboard bb = bb_color_pt(((pc[i] ^ cmirror) >> 3),
				(pc[i] & 0x07));
			do {
				p[i++] = pop_lsb(&bb);
			} while (bb);
		}
		idx = encode_piece(entry, entry->norm[bside], p, entry->factor[bside]);
		res = decompress_pairs(entry->precomp[bside], idx);
	}
	else {
		struct TBEntry_pawn *entry = (struct TBEntry_pawn *)ptr;
		int k = entry->file[0].pieces[0][0] ^ cmirror;
		Bitboard bb = bb_color_pt((k >> 3), (k & 0x07));
		i = 0;
		do {
			p[i++] = pop_lsb(&bb) ^ mirror;
		} while (bb);
		int f = pawn_file(entry, p);
		ubyte *pc = entry->file[f].pieces[bside];
		for (; i < entry->num;) {
			bb = bb_color_pt(((pc[i] ^ cmirror) >> 3),
				(pc[i] & 0x07));
			do {
				p[i++] = pop_lsb(&bb) ^ mirror;
			} while (bb);
		}
		idx = encode_pawn(entry, entry->file[f].norm[bside], p, entry->file[f].factor[bside]);
		res = decompress_pairs(entry->file[f].precomp[bside], idx);
	}

	return ((int)res) - 2;
}

static int probe_dtz_table(int wdl, int *success) {
	struct TBEntry *ptr;
	uint64 idx;
	int i, res;
	int p[TBPIECES];

	// Obtain the position's material signature key.
	uint64 key = calc_key(0);

	if (DTZ_table[0].key1 != key && DTZ_table[0].key2 != key) {
		for (i = 1; i < DTZ_ENTRIES; i++)
			if (DTZ_table[i].key1 == key) break;
		if (i < DTZ_ENTRIES) {
			struct DTZTableEntry table_entry = DTZ_table[i];
			for (; i > 0; i--)
				DTZ_table[i] = DTZ_table[i - 1];
			DTZ_table[0] = table_entry;
		}
		else {
			struct TBHashEntry *ptr2 = TB_hash[key >> (64 - TBHASHBITS)];
			for (i = 0; i < HSHMAX; i++)
				if (ptr2[i].key == key) break;
			if (i == HSHMAX) {
				*success = 0;
				return 0;
			}
			ptr = ptr2[i].ptr;
			char str[16];
			int mirror = (ptr->key != key);
			prt_str(str, mirror);
			if (DTZ_table[DTZ_ENTRIES - 1].entry)
				free_dtz_entry(DTZ_table[DTZ_ENTRIES - 1].entry);
			for (i = DTZ_ENTRIES - 1; i > 0; i--)
				DTZ_table[i] = DTZ_table[i - 1];
			load_dtz_table(str, calc_key(mirror), calc_key(!mirror));
		}
	}

	ptr = DTZ_table[0].entry;
	if (!ptr) {
		*success = 0;
		return 0;
	}

	int bside, mirror, cmirror;
	if (!ptr->symmetric) {
		if (key != ptr->key) {
			cmirror = 8;
			mirror = 0x38;
			bside = (Current->turn == WHITE);
		}
		else {
			cmirror = mirror = 0;
			bside = !(Current->turn == WHITE);
		}
	}
	else {
		cmirror = Current->turn == WHITE ? 0 : 8;
		mirror = Current->turn == WHITE ? 0 : 0x38;
		bside = 0;
	}

	if (!ptr->has_pawns) {
		struct DTZEntry_piece *entry = (struct DTZEntry_piece *)ptr;
		if ((entry->flags & 1) != bside && !entry->symmetric) {
			*success = -1;
			return 0;
		}
		ubyte *pc = entry->pieces;
		for (i = 0; i < entry->num;) {
			Bitboard bb = bb_color_pt(((pc[i] ^ cmirror) >> 3),
				(pc[i] & 0x07));
			do {
				p[i++] = pop_lsb(&bb);
			} while (bb);
		}
		idx = encode_piece((struct TBEntry_piece *)entry, entry->norm, p, entry->factor);
		res = decompress_pairs(entry->precomp, idx);

		if (entry->flags & 2)
			res = entry->map[entry->map_idx[wdl_to_map[wdl + 2]] + res];

		if (!(entry->flags & pa_flags[wdl + 2]) || (wdl & 1))
			res *= 2;
	}
	else {
		struct DTZEntry_pawn *entry = (struct DTZEntry_pawn *)ptr;
		int k = entry->file[0].pieces[0] ^ cmirror;
		Bitboard bb = bb_color_pt((k >> 3), (k & 0x07));
		i = 0;
		do {
			p[i++] = pop_lsb(&bb) ^ mirror;
		} while (bb);
		int f = pawn_file((struct TBEntry_pawn *)entry, p);
		if ((entry->flags[f] & 1) != bside) {
			*success = -1;
			return 0;
		}
		ubyte *pc = entry->file[f].pieces;
		for (; i < entry->num;) {
			bb = bb_color_pt(((pc[i] ^ cmirror) >> 3),
				(pc[i] & 0x07));
			do {
				p[i++] = pop_lsb(&bb) ^ mirror;
			} while (bb);
		}
		idx = encode_pawn((struct TBEntry_pawn *)entry, entry->file[f].norm, p, entry->file[f].factor);
		res = decompress_pairs(entry->file[f].precomp, idx);

		if (entry->flags[f] & 2)
			res = entry->map[entry->map_idx[f][wdl_to_map[wdl + 2]] + res];

		if (!(entry->flags[f] & pa_flags[wdl + 2]) || (wdl & 1))
			res *= 2;
	}

	return res;
}

static int probe_ab(int alpha, int beta, int *success) {
	int moves[64], move, v;

	Current->mask = Piece(Current->turn ^ 1);
	if (Current->turn == White) gen_captures<0, 1>(moves);
	else gen_captures<1, 1>(moves);

	for (int * p = moves; move = (*p) & 0xFFFF; p++) {
		int me = Current->turn;
		if (!Square(To(move)) || IsEP(move) || IsIllegal(me, move)) continue;
		if (Current->turn == White) do_move<0>(move);
		else do_move<1>(move);
		nodes--;
		compute_pos_info();
		if (Check(Current->turn ^ 1)) {
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			continue;
		}
		v = -probe_ab(-beta, -alpha, success);
		if (Current->turn == White) undo_move<1>(move);
		else undo_move<0>(move);
		if (*success == 0) return 0;
		if (v > alpha) {
			if (v >= beta) {
				*success = 2;
				return v;
			}
			alpha = v;
		}
	}

	v = probe_wdl_table(success);
	if (*success == 0) return 0;
	if (alpha >= v) {
		*success = 1 + (alpha > 0);
		return alpha;
	}
	else {
		*success = 1;
		return v;
	}
}

// Probe the WDL table for a particular position.
// If *success != 0, the probe was successful.
// The return value is from the point of view of the side to move:
// -2 : loss
// -1 : loss, but draw under 50-move rule
//  0 : draw
//  1 : win, but draw under 50-move rule
//  2 : win
int probe_wdl(int *success)
{
	int v;

	*success = 1;
	evaluate();
	v = probe_ab(-2, 2, success);

	// If en passant is not possible, we are done.
	if (!Current->ep_square)
		return v;
	if (!(*success)) return 0;

	// Now handle en passant.
	int v1 = -3;

	// Generate (at least) all legal en passant captures.
	int moves[64], move, me = Current->turn;
	int *p = moves;
	for (uint64 u = (Pawn(me) & PAtt[opp][Current->ep_square]); u; Cut(u)) {
		*p = (lsb(u) << 6) | Current->ep_square | FlagEP;
		p++;
	}
	*p = 0;

	for (p = moves; move = (*p) & 0xFFFF; p++) {
		if (IsIllegal(me, move)) continue;
		if (Current->turn == White) do_move<0>(move);
		else do_move<1>(move);
		nodes--;
		compute_pos_info();
		if (Check(Current->turn ^ 1)) {
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			continue;
		}
		int v0 = -probe_ab(-2, 2, success);
		if (Current->turn == White) undo_move<1>(move);
		else undo_move<0>(move);
		if (*success == 0) return 0;
		if (v0 > v1) v1 = v0;
	}

	if (v1 > -3) {
		if (v1 >= v) v = v1;
		else if (v == 0) {
			if (Check(me)) {
				if (!me) gen_evasions<0>(moves);
				else gen_evasions<1>(moves);
			}
			else {
				Current->mask = Piece(opp);
				if (!me) {
					p = gen_captures<0, 0>(moves);
					gen_quiet_moves<0>(p);
				}
				else {
					p = gen_captures<1, 0>(moves);
					gen_quiet_moves<1>(p);
				}
			}
			for (p = moves; move = (*p) & 0xFFFF; p++) {
				if (IsEP(move)) continue;
				if (IsIllegal(me, move)) continue;
				return v;
			}
			// If not, then we are forced to play the losing ep capture.
			return v1;
		}
	}

	return v;
}

static int probe_dtz_no_ep(int *success) {
	int wdl, dtz;

	evaluate();
	wdl = probe_ab(-2, 2, success);
	if (*success == 0) return 0;

	if (wdl == 0) return 0;

	if (*success == 2)
		return wdl == 2 ? 1 : 101;

	int moves[64], move, v;
	// Generate at least all legal moves
	Current->mask = Piece(Current->turn ^ 1);
	if (Current->turn == White) {
		int *p = gen_captures<0, 1>(moves);
		gen_quiet_moves<0>(p);
	}
	else {
		int * p = gen_captures<1, 1>(moves);
		gen_quiet_moves<1>(p);
	}

	if (wdl > 0) {
		for (int * p = moves; move = (*p) & 0xFFFF; p++) {
			int me = Current->turn;
			if (Square(From(move)) >= WhiteKnight || IsEP(move) || IsIllegal(me, move) || Square(To(move))) continue;
			if (Current->turn == White) do_move<0>(move);
			else do_move<1>(move);
			nodes--;
			compute_pos_info();
			if (Check(Current->turn ^ 1)) {
				if (Current->turn == White) undo_move<1>(move);
				else undo_move<0>(move);
				continue;
			}
			v = -probe_ab(-2, -wdl + 1, success);
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			if (*success == 0) return 0;
			if (v == wdl)
				return v == 2 ? 1 : 101;
		}
	}

	dtz = 1 + probe_dtz_table(wdl, success);
	if (*success >= 0) {
		if (wdl & 1) dtz += 100;
		return wdl >= 0 ? dtz : -dtz;
	}

	if (wdl > 0) {
		int best = 0xffff;
		for (int * p = moves; move = (*p) & 0xFFFF; p++) {
			int me = Current->turn;
			if (Square(From(move)) < WhiteKnight || IsEP(move) || IsIllegal(me, move) || Square(To(move))) continue;
			if (Current->turn == White) do_move<0>(move);
			else do_move<1>(move);
			nodes--;
			compute_pos_info();
			if (Check(Current->turn ^ 1)) {
				if (Current->turn == White) undo_move<1>(move);
				else undo_move<0>(move);
				continue;
			}
			v = -probe_dtz(success);
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			if (*success == 0) return 0;
			if (v > 0 && v + 1 < best)
				best = v + 1;
		}
		return best;
	}
	else {
		int best = -1;
		for (int * p = moves; move = (*p) & 0xFFFF; p++) {
			int me = Current->turn;
			if (IsIllegal(me, move)) continue;
			if (Current->turn == White) do_move<0>(move);
			else do_move<1>(move);
			nodes--;
			compute_pos_info();
			if (Check(Current->turn ^ 1)) {
				if (Current->turn == White) undo_move<1>(move);
				else undo_move<0>(move);
				continue;
			}
			if (!Current->ply) {
				if (wdl == -2) v = -1;
				else {
					v = probe_ab(1, 2, success);
					v = (v == 2) ? 0 : -101;
				}
			}
			else {
				v = -probe_dtz(success) - 1;
			}
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			if (*success == 0) return 0;
			if (v < best)
				best = v;
		}
		return best;
	}
}

static int wdl_to_dtz[] = {
	-1, -101, 0, 101, 1
};

// Probe the DTZ table for a particular position.
// If *success != 0, the probe was successful.
// The return value is from the point of view of the side to move:
// n < -100 : loss, but draw under 50-move rule
// -100 <= n < -1 : loss in n ply (assuming 50-move counter == 0)
// 0 : draw
// 1 < n <= 100 : win in n ply (assuming 50-move counter == 0)
// 100 < n : win, but draw under 50-move rule
//
// The return value n can be off by 1: a return value -n can mean a loss
// in n+1 ply and a return value +n can mean a win in n+1 ply. This
// cannot happen for tables with positions exactly on the "edge" of
// the 50-move rule.
//
// This implies that if dtz > 0 is returned, the position is certainly
// a win if dtz + 50-move-counter <= 99. Care must be taken that the ENGINE
// picks moves that preserve dtz + 50-move-counter <= 99.
//
// If n = 100 immediately after a capture or pawn move, then the position
// is also certainly a win, and during the whole phase until the next
// capture or pawn move, the inequality to be preserved is
// dtz + 50-movecounter <= 100.
//
// In short, if a move is available resulting in dtz + 50-move-counter <= 99,
// then do not accept moves leading to dtz + 50-move-counter == 100.
//
int probe_dtz(int *success) {
	*success = 1;
	int v = probe_dtz_no_ep(success);

	if (!Current->ep_square)
		return v;
	if (*success == 0) return 0;

	// Now handle en passant.
	int v1 = -3;

	// Generate (at least) all legal en passant captures.
	int moves[64], move, me = Current->turn;
	int *p = moves;
	for (uint64 u = (Pawn(me) & PAtt[opp][Current->ep_square]); u; Cut(u)) {
		*p = (lsb(u) << 6) | Current->ep_square | FlagEP;
		p++;
	}
	*p = 0;

	for (p = moves; move = (*p) & 0xFFFF; p++) {
		if (IsIllegal(me, move)) continue;
		if (Current->turn == White) do_move<0>(move);
		else do_move<1>(move);
		nodes--;
		compute_pos_info();
		if (Check(Current->turn ^ 1)) {
			if (Current->turn == White) undo_move<1>(move);
			else undo_move<0>(move);
			continue;
		}
		int v0 = -probe_ab(-2, 2, success);
		if (Current->turn == White) undo_move<1>(move);
		else undo_move<0>(move);
		if (*success == 0) return 0;
		if (v0 > v1) v1 = v0;
	}

	if (v1 > -3) {
		v1 = wdl_to_dtz[v1 + 2];
		if (v < -100) {
			if (v1 >= 0)
				v = v1;
		}
		else if (v < 0) {
			if (v1 >= 0 || v1 < 100)
				v = v1;
		}
		else if (v > 100) {
			if (v1 > 0)
				v = v1;
		}
		else if (v > 0) {
			if (v1 == 1)
				v = v1;
		}
		else if (v1 >= 0) {
			v = v1;
		}
		else {
			if (Check(me)) {
				if (!me) gen_evasions<0>(moves);
				else gen_evasions<1>(moves);
			}
			else {
				Current->mask = Piece(opp);
				if (!me) {
					p = gen_captures<0, 0>(moves);
					gen_quiet_moves<0>(p);
				}
				else {
					p = gen_captures<1, 0>(moves);
					gen_quiet_moves<1>(p);
				}
			}
			for (p = moves; move = (*p) & 0xFFFF; p++) {
				if (IsEP(move)) continue;
				if (IsIllegal(me, move)) continue;
				return v;
			}
			// If not, then we are forced to play the losing ep capture.
			return v1;
		}
	}

	return v;
}



#endif // End TBPROBE Syzygy tbprobe code 

// Next back Gull code





template <bool me> int is_check(int move) { // doesn't detect castling and ep checks
	uint64 king;
	int from, to, piece, king_sq;

	from = From(move);
	to = To(move);
	king = King(opp);
	king_sq = lsb(king);
	piece = Square(from);
	if (T(Bit(from) & Current->xray[me]) && F(FullLine[king_sq][from] & Bit(to))) return 1;
	if (piece < WhiteKnight) {
		if (PAtt[me][to] & king) return 1;
		if (T(Bit(to) & Line(me, 7)) && T(king & Line(me, 7)) && F(Between[to][king_sq] & PieceAll)) return 1;
	} else if (piece < WhiteLight) {
		if (NAtt[to] & king) return 1;
	} else if (piece < WhiteRook) {
		if (BMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteQueen) {
		if (RMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteKing) {
		if (QMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	}
	return 0;
}

void hash_high(int value, int depth) {
	int i, score, min_score;
	GEntry *best, *Entry;

	min_score = 0x70000000;
	for (i = 0, best = Entry = Hash + (High32(Current->key) & hash_mask); i < 4; i++, Entry++) {
		if (Entry->key == Low32(Current->key)) {
			Entry->date = date;
			if (depth > Entry->high_depth || (depth == Entry->high_depth && value < Entry->high)) {
				if (Entry->low <= value) { 
				    Entry->high_depth = depth;
				    Entry->high = value;
				} else if (Entry->low_depth < depth) {
					Entry->high_depth = depth;
				    Entry->high = value;
					Entry->low = value;
				}
			}
			return;
		} else score = (Convert(Entry->date,int) << 3) + Convert(Max(Entry->high_depth, Entry->low_depth),int);
		if (score < min_score) {
			min_score = score;
			best = Entry;
		}
	}
	best->date = date;
	best->key = Low32(Current->key);
	best->high = value;
	best->high_depth = depth;
	best->low = 0;
	best->low_depth = 0;
	best->move = 0;
	best->flags = 0;
	return;
}

void hash_low(int move, int value, int depth) {
	int i, score, min_score;
	GEntry *best, *Entry;

	min_score = 0x70000000;
	move &= 0xFFFF;
	for (i = 0, best = Entry = Hash + (High32(Current->key) & hash_mask); i < 4; i++, Entry++) {
		if (Entry->key == Low32(Current->key)) {
			Entry->date = date;
			if (depth > Entry->low_depth || (depth == Entry->low_depth && value > Entry->low)) {
				if (move) Entry->move = move;
				if (Entry->high >= value) {
				    Entry->low_depth = depth;
				    Entry->low = value;
				} else if (Entry->high_depth < depth) {
					Entry->low_depth = depth;
				    Entry->low = value;
					Entry->high = value;
				}
			} else if (F(Entry->move)) Entry->move = move;
			return;
		} else score = (Convert(Entry->date,int) << 3) + Convert(Max(Entry->high_depth, Entry->low_depth),int);
		if (score < min_score) {
			min_score = score;
			best = Entry;
		}
	}
	best->date = date;
	best->key = Low32(Current->key);
	best->high = 0;
	best->high_depth = 0;
	best->low = value;
	best->low_depth = depth;
	best->move = move;
	best->flags = 0;
	return;
}

void hash_exact(int move, int value, int depth, int exclusion, int ex_depth, int knodes) {
	int i, score, min_score;
	GPVEntry *best;
	GPVEntry * PVEntry;

	min_score = 0x70000000;
	for (i = 0, best = PVEntry = PVHash + (High32(Current->key) & pv_hash_mask); i < pv_cluster_size; i++, PVEntry++) {
		if (PVEntry->key == Low32(Current->key)) {
			PVEntry->date = date;
			PVEntry->knodes += knodes;
			if (PVEntry->depth <= depth) {
				PVEntry->value = value;
				PVEntry->depth = depth;
				PVEntry->move = move;
				PVEntry->ply = Current->ply;
				if (ex_depth) {
					PVEntry->exclusion = exclusion;
					PVEntry->ex_depth = ex_depth;
				}
			}
			return;
		}
		score = (Convert(PVEntry->date,int) << 3) + Convert(PVEntry->depth,int);
		if (score < min_score) {
			min_score = score;
			best = PVEntry;
		}
	}
	best->key = Low32(Current->key);
	best->date = date;
	best->value = value;
	best->depth = depth;
	best->move = move;
	best->exclusion = exclusion;
	best->ex_depth = ex_depth;
	best->knodes = knodes;
	best->ply = Current->ply;
}

template <bool pv> __forceinline int extension(int move, int depth) {
	register int ext = 0;
	if (pv) {
		if (T(Current->passer & Bit(From(move))) && CRank(Current->turn, From(move)) >= 5 && depth < 16) ext = 2;
	} else {
		if (T(Current->passer & Bit(From(move))) && CRank(Current->turn, From(move)) >= 5 && depth < 16) ext = 1; 
	}
	return ext;
}

void sort(int * start, int * finish) {
	for (int * p = start; p < finish - 1; p++) {
		int * best = p;
		int value = *p;
		int previous = *p;
		for (int * q = p + 1; q < finish; q++) if ((*q) > value) {
			value = *q;
			best = q;
		}
		*best = previous;
		*p = value;
	}
}

void sort_moves(int * start, int * finish) {
	for (int * p = start + 1; p < finish; p++) for (int * q = p - 1; q >= start; q--) if (((*q) >> 16) < ((*(q+1)) >> 16)) {
		int move = *q;
		*q = *(q+1);
		*(q+1)=move;
	}
}

__forceinline int pick_move() {
	register int move, *p, *best;
	move = *(Current->current);
	if (F(move)) return 0;
	best = Current->current;
	for (p = Current->current + 1; T(*p); p++) {
		if ((*p) > move) {
			best = p;
			move = *p;
		}
	}
	*best = *(Current->current);
	*(Current->current) = move;
	Current->current++;
	return move & 0xFFFF;
}

template <bool me> void gen_next_moves() {
	int *p, *q, *r;
	Current->gen_flags &= ~FlagSort;
	switch (Current->stage) {
	case s_hash_move: case r_hash_move: case e_hash_move:
		Current->moves[0] = Current->killer[0];
		Current->moves[1] = 0;
		return;
	case s_good_cap: 
		Current->mask = Piece(opp);
		r = gen_captures<me, 0>(Current->moves);
		for (q = r - 1, p = Current->moves; q >= p;) {
		    int move = (*q) & 0xFFFF;
		    if (!see<me>(move,0)) {
			    int next = *p;
			    *p = *q;
			    *q = next;
			    p++;
		    } else q--;
	    }
		Current->start = p;
		Current->current = p;
		sort(p, r);
		return;
	case s_special:
		Current->current = Current->start;
		p = Current->start;
		if (Current->killer[1]) {*p = Current->killer[1]; p++;}
		if (Current->killer[2]) {*p = Current->killer[2]; p++;}
		if (Current->ref[0] && Current->ref[0] != Current->killer[1] && Current->ref[0] != Current->killer[2]) {*p = Current->ref[0]; p++;}
		if (Current->ref[1] && Current->ref[1] != Current->killer[1] && Current->ref[1] != Current->killer[2]) {*p = Current->ref[1]; p++;}
		*p = 0;
		return;
	case s_quiet: 
		gen_quiet_moves<me>(Current->start);
		Current->current = Current->start;
		Current->gen_flags |= FlagSort;
		return;
	case s_bad_cap:
		*(Current->start) = 0;
		Current->current = Current->moves;
		if (!(Current->gen_flags & FlagNoBcSort)) sort(Current->moves, Current->start);
		return;
	case r_cap:
		r = gen_captures<me, 0>(Current->moves);
		Current->current = Current->moves;
		sort(Current->moves, r);
		return;
	case r_checks:
		r = gen_checks<me>(Current->moves);
		Current->current = Current->moves; 
		sort(Current->moves, r);
		return;
	case e_ev:
		Current->mask = Filled;
		r = gen_evasions<me>(Current->moves);
		mark_evasions(Current->moves);
		sort(Current->moves, r);
		Current->current = Current->moves;
		return;
	}
}

template <bool me, bool root> int get_move() {
	int move;
	
	if (root) {
		move = (*Current->current) & 0xFFFF;
		Current->current++;
		return move;
	}
start:
	if (F(*Current->current)) {
		Current->stage++;
		if ((1 << Current->stage) & StageNone) return 0;
		gen_next_moves<me>();
		goto start;
	}
	if (Current->gen_flags & FlagSort) move = pick_move();
	else {
		move = (*Current->current) & 0xFFFF;
		Current->current++;
	}
	if (Current->stage == s_quiet) { 
		if (move == Current->killer[1] || move == Current->killer[2] || move == Current->ref[0] || move == Current->ref[1]) goto start;
	} else if (Current->stage == s_special && (Square(To(move)) || !is_legal<me>(move))) goto start;
	return move;
}

template <bool me> int see(int move, int margin) {
	int from, to, piece, capture, delta, sq, pos;
	uint64 clear, def, att, occ, b_area, r_slider_att, b_slider_att, r_slider_def, b_slider_def, r_area, u, new_att, my_bishop, opp_bishop;
	from = From(move);
	to = To(move);
	piece = SeeValue[Square(from)];
	capture = SeeValue[Square(to)];
	delta = piece - capture;
	if (delta <= -margin) return 1;
	if (piece == SeeValue[WhiteKing]) return 1;
	if (Current->xray[me] & Bit(from)) return 1;
	if (T(Current->pin[me] & Bit(from)) && piece <= SeeValue[WhiteDark]) return 1;
	if (piece > (SeeValue[WhiteKing] >> 1)) return 1;
	if (IsEP(move)) return 1;
	if (F(Current->att[opp] & Bit(to))) return 1;
	att = PAtt[me][to] & Pawn(opp);
	if (T(att) && delta + margin > SeeValue[WhitePawn]) return 0;
	clear = ~Bit(from);
	def = PAtt[opp][to] & Pawn(me) & clear;
	if (T(def) && delta + SeeValue[WhitePawn] + margin <= 0) return 1;
	att |= NAtt[to] & Knight(opp);
	if (T(att) && delta > SeeValue[WhiteDark] - margin) return 0;
	occ = PieceAll & clear;
    b_area = BishopAttacks(to,occ);
	opp_bishop = Bishop(opp);
	if (delta > SeeValue[IDark(me)] - margin) if (b_area & opp_bishop) return 0;
	my_bishop = Bishop(me);
    b_slider_att = BMask[to] & (opp_bishop | Queen(opp));
	r_slider_att = RMask[to] & Major(opp);
	b_slider_def = BMask[to] & (my_bishop | Queen(me)) & clear;
	r_slider_def = RMask[to] & Major(me) & clear;
	att |= (b_slider_att & b_area);
	def |= NAtt[to] & Knight(me) & clear;
	r_area = RookAttacks(to,occ);
	att |= (r_slider_att & r_area);
	def |= (b_slider_def & b_area);
	def |= (r_slider_def & r_area);
	att |= SArea[to] & King(opp);
	def |= SArea[to] & King(me) & clear;
	while (true) {
		if (u = (att & Pawn(opp))) {
			capture -= piece;
			piece = SeeValue[WhitePawn];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Knight(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteKnight];
			att ^= (~(u-1)) & u;
		} else if (u = (att & opp_bishop)) {
            capture -= piece;
			piece = SeeValue[WhiteDark];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Rook(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteRook];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Queen(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_att | b_slider_att) & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & King(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteKing];
		} else return 1;
		if (capture < -(SeeValue[WhiteKing] >> 1)) return 0;
		if (piece + capture < margin) return 0;
		if (u = (def & Pawn(me))) {
            capture += piece;
			piece = SeeValue[WhitePawn];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Knight(me))) {
            capture += piece;
			piece = SeeValue[WhiteKnight];
			def ^= (~(u-1)) & u;
		} else if (u = (def & my_bishop)) {
            capture += piece;
			piece = SeeValue[WhiteDark];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Rook(me))) {
            capture += piece;
			piece = SeeValue[WhiteRook];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Queen(me))) {
            capture += piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_def | b_slider_def) & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & King(me))) {
            capture += piece;
			piece = SeeValue[WhiteKing];
		} else return 0;
		if (capture > (SeeValue[WhiteKing] >> 1)) return 1;
		if (capture - piece >= margin) return 1;
	}
}

template <bool me> void gen_root_moves() {
	int i, *p, killer, depth = -256, move;
	GEntry * Entry;
	GPVEntry * PVEntry;

	killer = 0;
	if (Entry = probe_hash()) {
		if (T(Entry->move) && Entry->low_depth > depth) {
			depth = Entry->low_depth;
			killer = Entry->move;
		}
	}
	if (PVEntry = probe_pv_hash()) {
		if (PVEntry->depth > depth && T(PVEntry->move)) {
			depth = PVEntry->depth;
			killer = PVEntry->move;
		}
	}

	Current->killer[0] = killer;
	if (Check(me)) Current->stage = stage_evasion;
	else {
		Current->stage = stage_search;
		Current->ref[0] = RefM(Current->move).ref[0];
	    Current->ref[1] = RefM(Current->move).ref[1];
	}
	Current->gen_flags = 0;
	p = RootList;
	Current->current = Current->moves;
	Current->moves[0] = 0;
	while (move = get_move<me,0>()) {
		if (IsIllegal(me,move)) continue;
		if (p > RootList && move == killer) continue;
		if (SearchMoves) {
			for (i = 0; i < SMPointer; i++)
				if (SMoves[i] == move) goto keep_move;
			continue;
	    }
keep_move:
		*p = move;
		p++;
	}
	*p = 0;
}

template <bool me, bool up> int * gen_captures(int * list) {
	uint64 u, v;

	if (Current->ep_square)
		for (v = PAtt[opp][Current->ep_square] & Pawn(me); T(v); Cut(v)) AddMove(lsb(v),Current->ep_square,FlagEP,MvvLva[IPawn(me)][IPawn(opp)])
	for (u = Pawn(me) & Line(me,6); T(u); Cut(u))
    	if (F(Square(lsb(u) + Push(me)))) {
			AddMove(lsb(u),lsb(u) + Push(me),FlagPQueen,MvvLvaPromotion)
			if (NAtt[lsb(King(opp))] & Bit(lsb(u) + Push(me))) AddMove(lsb(u),lsb(u) + Push(me),FlagPKnight,MvvLvaPromotionKnight)
		}
	for (v = ShiftW(opp,Current->mask) & Pawn(me) & Line(me,6); T(v); Cut(v)) {
		AddMove(lsb(v),lsb(v)+PushE(me),FlagPQueen,MvvLvaPromotionCap(Square(lsb(v)+PushE(me))))
		if (NAtt[lsb(King(opp))] & Bit(lsb(v) + PushE(me))) AddMove(lsb(v),lsb(v)+PushE(me),FlagPKnight,MvvLvaPromotionKnightCap(Square(lsb(v)+PushE(me))))
	}
	for (v = ShiftE(opp,Current->mask) & Pawn(me) & Line(me,6); T(v); Cut(v)) {
		AddMove(lsb(v),lsb(v)+PushW(me),FlagPQueen,MvvLvaPromotionCap(Square(lsb(v)+PushW(me))))
		if (NAtt[lsb(King(opp))] & Bit(lsb(v) + PushW(me))) AddMove(lsb(v),lsb(v)+PushW(me),FlagPKnight,MvvLvaPromotionKnightCap(Square(lsb(v)+PushE(me))))
	}
	if (F(Current->att[me] & Current->mask)) goto finish;
	for (v = ShiftW(opp,Current->mask) & Pawn(me) & (~Line(me,6)); T(v); Cut(v)) AddCaptureP(IPawn(me),lsb(v),lsb(v)+PushE(me),0)
	for (v = ShiftE(opp,Current->mask) & Pawn(me) & (~Line(me,6)); T(v); Cut(v)) AddCaptureP(IPawn(me),lsb(v),lsb(v)+PushW(me),0)
	for (v = SArea[lsb(King(me))] & Current->mask & (~Current->att[opp]); T(v); Cut(v)) AddCaptureP(IKing(me),lsb(King(me)),lsb(v),0)
	for (u = Knight(me); T(u); Cut(u))
		for (v = NAtt[lsb(u)] & Current->mask; T(v); Cut(v)) AddCaptureP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = BishopAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCapture(lsb(u),lsb(v),0)
	for (u = Rook(me); T(u); Cut(u))
		for (v = RookAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCaptureP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = QueenAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCaptureP(IQueen(me),lsb(u),lsb(v),0)
finish:
	*list = 0;
	return list;
}

template <bool me> int * gen_evasions(int * list) {
	int king, att_sq, from;
	uint64 att, esc, b, u;

	king = lsb(King(me));
	att = (NAtt[king] & Knight(opp)) | (PAtt[me][king] & Pawn(opp));
	for (u = (BMask[king] & BSlider(opp)) | (RMask[king] & RSlider(opp)); T(u); u ^= b) {
		b = Bit(lsb(u));
		if (F(Between[king][lsb(u)] & PieceAll)) att |= b;
	}
	att_sq = lsb(att);
	esc = SArea[king] & (~(Piece(me) | Current->att[opp])) & Current->mask;
	if (Square(att_sq) >= WhiteLight) esc &= ~FullLine[king][att_sq];
	Cut(att);
	if (att) {
		att_sq = lsb(att);
		if (Square(att_sq) >= WhiteLight) esc &= ~FullLine[king][att_sq];
		for (; T(esc); Cut(esc)) AddCaptureP(IKing(me),king,lsb(esc),0)
		*list = 0;
		return list;
	}
	if (Bit(att_sq) & Current->mask) {
	    if (T(Current->ep_square) && Current->ep_square == att_sq + Push(me))
		    for (u = PAtt[opp][att_sq + Push(me)] & Pawn(me); T(u); Cut(u)) AddMove(lsb(u),att_sq + Push(me),FlagEP,MvvLva[IPawn(me)][IPawn(opp)])
	}
	for (u = PAtt[opp][att_sq] & Pawn(me); T(u); Cut(u)) {
        from = lsb(u);
		if (Bit(att_sq) & Line(me,7)) AddMove(from,att_sq,FlagPQueen,MvvLvaPromotionCap(Square(att_sq)))
		else if (Bit(att_sq) & Current->mask) AddCaptureP(IPawn(me),from,att_sq,0)
	}
	for ( ; T(esc); Cut(esc)) AddCaptureP(IKing(me),king,lsb(esc),0)
	att = Between[king][att_sq];
	for (u = Shift(opp,att) & Pawn(me); T(u); Cut(u)) {
        from = lsb(u);
		if (Bit(from) & Line(me,6)) AddMove(from,from + Push(me),FlagPQueen,MvvLvaPromotion)
		else if (F(~Current->mask)) AddMove(from,from + Push(me),0,0)
	}
	if (F(~Current->mask)) {
	    for (u = Shift(opp,Shift(opp,att)) & Line(me, 1) & Pawn(me); T(u); Cut(u))
            if (F(Square(lsb(u)+Push(me)))) AddMove(lsb(u),lsb(u) + 2 * Push(me),0,0)
    }
	att |= Bit(att_sq);
	for (u = Knight(me); T(u); Cut(u))
        for (esc = NAtt[lsb(u)] & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IKnight(me),lsb(u),lsb(esc),0)
		}
	for (u = Bishop(me); T(u); Cut(u))
        for (esc = BishopAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCapture(lsb(u),lsb(esc),0)
		}
	for (u = Rook(me); T(u); Cut(u))
        for (esc = RookAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IRook(me),lsb(u),lsb(esc),0)
		}
	for (u = Queen(me); T(u); Cut(u))
        for (esc = QueenAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IQueen(me),lsb(u),lsb(esc),0)
		}
	*list = 0;
	return list;
}

void mark_evasions(int * list) {
	for (; T(*list); list++) {
		register int move = (*list) & 0xFFFF;
	    if (F(Square(To(move))) && F(move & 0xE000)) {
			if (move == Current->ref[0]) *list |= RefOneScore;
			else if (move == Current->ref[1]) *list |= RefTwoScore;
			else if (move == Current->killer[1]) *list |= KillerOneScore;
			else if (move == Current->killer[2]) *list |= KillerTwoScore;
			else *list |= HistoryP(Square(From(move)),From(move),To(move));
		}
	}
}

template <bool me> int * gen_quiet_moves(int * list) {
	int to;
	uint64 u, v, free, occ;

    occ = PieceAll;
	free = ~occ;
	if (me == White) {
		if (T(Current->castle_flags & CanCastle_OO) && F(occ & 0x60) && F(Current->att[Black] & 0x70)) AddHistoryP(IKing(White),4,6,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_OOO) && F(occ & 0xE) && F(Current->att[Black] & 0x1C)) AddHistoryP(IKing(White),4,2,FlagCastling)
	} else {
		if (T(Current->castle_flags & CanCastle_oo) && F(occ & 0x6000000000000000) && F(Current->att[White] & 0x7000000000000000)) AddHistoryP(IKing(Black),60,62,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_ooo) && F(occ & 0x0E00000000000000) && F(Current->att[White] & 0x1C00000000000000)) AddHistoryP(IKing(Black),60,58,FlagCastling)
	}
	for (v = Shift(me,Pawn(me)) & free & (~Line(me,7)); T(v); Cut(v)) {
        to = lsb(v);
	    if (T(Bit(to) & Line(me,2)) && F(Square(to + Push(me)))) AddHistoryP(IPawn(me),to - Push(me),to + Push(me),0)
		AddHistoryP(IPawn(me),to - Push(me),to,0)
	}
	for (u = Knight(me); T(u); Cut(u))
		for (v = free & NAtt[lsb(u)]; T(v); Cut(v)) AddHistoryP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = free & BishopAttacks(lsb(u),occ); T(v); Cut(v)) AddHistory(lsb(u),lsb(v))
	for (u = Rook(me); T(u); Cut(u))
		for (v = free & RookAttacks(lsb(u),occ); T(v); Cut(v)) AddHistoryP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = free & QueenAttacks(lsb(u),occ); T(v); Cut(v)) AddHistoryP(IQueen(me),lsb(u),lsb(v),0)
	for (v = SArea[lsb(King(me))] & free & (~Current->att[opp]); T(v); Cut(v)) AddHistoryP(IKing(me),lsb(King(me)),lsb(v),0)
	*list = 0;
	return list;
}

template <bool me> int * gen_checks(int * list) {
	int king, from;
    uint64 u, v, target, b_target, r_target, clear, xray;

	clear = ~(Piece(me) | Current->mask);
    king = lsb(King(opp));
	for (u = Current->xray[me] & Piece(me); T(u); Cut(u)) {
		from = lsb(u);
		target = clear & (~FullLine[king][from]);
		if (Square(from) == IPawn(me)) {
			if (F(Bit(from + Push(me)) & Line(me,7))) {
			    if (T(Bit(from + Push(me)) & target) && F(Square(from + Push(me)))) AddMove(from,from + Push(me),0,MvvLvaXray)
				for (v = PAtt[me][from] & target & Piece(opp); T(v); Cut(v)) AddMove(from,lsb(v),0,MvvLvaXrayCap(Square(lsb(v))))
			}
		} else {
			if (Square(from) < WhiteLight) v = NAtt[from] & target;
			else if (Square(from) < WhiteRook) v = BishopAttacks(from,PieceAll) & target;
			else if (Square(from) < WhiteQueen) v = RookAttacks(from,PieceAll) & target;
			else if (Square(from) < WhiteKing) v = QueenAttacks(from,PieceAll) & target;
			else v = SArea[from] & target & (~Current->att[opp]);
			for ( ; T(v); Cut(v)) AddMove(from,lsb(v),0,MvvLvaXrayCap(Square(lsb(v))))
		}
	}
	xray = ~(Current->xray[me] & Board->bb[me]);
	for (u = Knight(me) & NArea[king] & xray; T(u); Cut(u))
		for (v = NAtt[king] & NAtt[lsb(u)] & clear; T(v); Cut(v)) AddCaptureP(IKnight(me),lsb(u),lsb(v),0)
    for (u = DArea[king] & Pawn(me) & (~Line(me,6)) & xray; T(u); Cut(u)) {
		from = lsb(u);
		for (v = PAtt[me][from] & PAtt[opp][king] & clear & Piece(opp); T(v); Cut(v)) AddCaptureP(IPawn(me),from,lsb(v),0)
		if (F(Square(from + Push(me))) && T(Bit(from + Push(me)) & PAtt[opp][king])) AddMove(from,from + Push(me),0,0)
	}
	b_target = BishopAttacks(king,PieceAll) & clear;
	r_target = RookAttacks(king,PieceAll) & clear;
	for (u = (Odd(king ^ Rank(king)) ? Board->bb[WhiteLight | me] : Board->bb[WhiteDark | me]) & xray; T(u); Cut(u))
		for (v = BishopAttacks(lsb(u),PieceAll) & b_target; T(v); Cut(v)) AddCapture(lsb(u),lsb(v),0)
	for (u = Rook(me) & xray; T(u); Cut(u)) 
		for (v = RookAttacks(lsb(u),PieceAll) & r_target; T(v); Cut(v)) AddCaptureP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me) & xray; T(u); Cut(u)) 
		for (v = QueenAttacks(lsb(u),PieceAll) & (b_target | r_target); T(v); Cut(v)) AddCaptureP(IQueen(me),lsb(u),lsb(v),0)
	*list = 0;
	return list;
}

template <bool me> int * gen_delta_moves(int * list) {
	int to;
	uint64 u, v, free, occ;

    occ = PieceAll;
	free = ~occ;
	if (me == White) {
		if (T(Current->castle_flags & CanCastle_OO) && F(occ & 0x60) && F(Current->att[Black] & 0x70)) AddCDeltaP(IKing(White),4,6,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_OOO) && F(occ & 0xE) && F(Current->att[Black] & 0x1C)) AddCDeltaP(IKing(White),4,2,FlagCastling)
	} else {
		if (T(Current->castle_flags & CanCastle_oo) && F(occ & 0x6000000000000000) && F(Current->att[White] & 0x7000000000000000)) AddCDeltaP(IKing(Black),60,62,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_ooo) && F(occ & 0x0E00000000000000) && F(Current->att[White] & 0x1C00000000000000)) AddCDeltaP(IKing(Black),60,58,FlagCastling)
	}
	for (v = Shift(me,Pawn(me)) & free & (~Line(me,7)); T(v); Cut(v)) {
        to = lsb(v);
	    if (T(Bit(to) & Line(me,2)) && F(Square(to + Push(me)))) AddCDeltaP(IPawn(me),to - Push(me),to + Push(me),0)
		AddCDeltaP(IPawn(me),to - Push(me),to,0)
	}
	for (u = Knight(me); T(u); Cut(u))
		for (v = free & NAtt[lsb(u)]; T(v); Cut(v)) AddCDeltaP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = free & BishopAttacks(lsb(u),occ); T(v); Cut(v)) AddCDelta(lsb(u),lsb(v))
	for (u = Rook(me); T(u); Cut(u))
		for (v = free & RookAttacks(lsb(u),occ); T(v); Cut(v)) AddCDeltaP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = free & QueenAttacks(lsb(u),occ); T(v); Cut(v)) AddCDeltaP(IQueen(me),lsb(u),lsb(v),0)
	for (v = SArea[lsb(King(me))] & free & (~Current->att[opp]); T(v); Cut(v)) AddCDeltaP(IKing(me),lsb(King(me)),lsb(v),0)
	*list = 0;
	return list;
}

template <bool me> int singular_extension(int ext, int prev_ext, int margin_one, int margin_two, int depth, int killer) {
	int value = -MateValue;
	int singular = 0;
	if (ext < 1 + (prev_ext < 1)) {
		if (Check(me)) value = search_evasion<me, 1>(margin_one, depth, killer); 
		else value = search<me, 1>(margin_one, depth, killer); 
		if (value < margin_one) singular = 1;
	}
	if (value < margin_one && ext < 2 + (prev_ext < 1) - (prev_ext >= 2)) {
		if (Check(me)) value = search_evasion<me, 1>(margin_two, depth, killer); 
		else value = search<me, 1>(margin_two, depth, killer); 
		if (value < margin_two) singular = 2;
	}
	return singular;
}

template <bool me> __forceinline void capture_margin(int alpha, int &score) {
	if (Current->score + 200 < alpha) {
		if (Current->att[me] & Pawn(opp)) {
			Current->mask ^= Pawn(opp);
			score = Current->score + 200;
		}
		if (Current->score + 500 < alpha) {
			if (Current->att[me] & Minor(opp)) {
				Current->mask ^= Minor(opp);
				score = Current->score + 500;
			}
			if (Current->score + 700 < alpha) {
				if (Current->att[me] & Rook(opp)) {
					Current->mask ^= Rook(opp);
					score = Current->score + 700;
				}
				if (Current->score + 1400 < alpha && (Current->att[me] & Queen(opp))) {
					Current->mask ^= Queen(opp);
					score = Current->score + 1400;
				}
			}
		}
	}
}

template <bool me, bool pv> int q_search(int alpha, int beta, int depth, int flags) {
	int i, value, score, move, hash_move, hash_depth, cnt;
	GEntry * Entry;

	if (flags & FlagHaltCheck) halt_check;
#ifdef CPU_TIMING
#ifndef TIMING
	if (nodes > check_node + 0x4000) {
#else
	if (nodes > check_node + 0x100) {
#endif
		check_node = nodes;
#ifdef TIMING
		if (LastDepth >= 6)
#endif
		check_time(1);

	}
#endif


#ifdef TB1 //possible entry TB

#endif

	if (flags & FlagCallEvaluation) evaluate();
	if (Check(me)) return q_evasion<me, pv>(alpha, beta, depth, FlagHashCheck);
	score = Current->score + 3;
	if (score > alpha) {
		alpha = score;
		if (score >= beta) return score;
	}

	hash_move = hash_depth = 0;
	if (flags & FlagHashCheck) {
	    for (i = 0, Entry = Hash + (High32(Current->key) & hash_mask); i < 4; Entry++, i++) {
		    if (Low32(Current->key) == Entry->key) {
			    if (T(Entry->low_depth)) {
				    if (Entry->low >= beta && !pv) return Entry->low;
				    if (Entry->low_depth > hash_depth && T(Entry->move)) {
					    hash_move = Entry->move;
					    hash_depth = Entry->low_depth;
				    }
			    }
			    if (T(Entry->high_depth) && Entry->high <= alpha && !pv) return Entry->high;
				break;
		    }
	    }
	}

	Current->mask = Piece(opp);
	capture_margin<me>(alpha, score);

	cnt = 0;
	if (T(hash_move)) {
		if (F(Bit(To(hash_move)) & Current->mask) && F(hash_move & 0xE000) && (depth < -8 || (Current->score + DeltaM(hash_move) <= alpha && F(is_check<me>(hash_move))))) goto skip_hash_move;
		if (is_legal<me>(move = hash_move)) {
			if (IsIllegal(me,move)) goto skip_hash_move;
			if (SeeValue[Square(To(move))] > SeeValue[Square(From(move))]) cnt++;
			do_move<me>(move);
		    value = -q_search<opp, pv>(-beta, -alpha, depth - 1, FlagNeatSearch);
		    undo_move<me>(move);
			if (value > score) {
			    score = value;
			    if (value > alpha) {
				    alpha = value;
			        if (value >= beta) goto cut;
			    }
		    }
			if (F(Bit(To(hash_move)) & Current->mask) && F(hash_move & 0xE000) && (depth < -2 || depth <= -1 && Current->score + 50 < alpha) && alpha >= beta - 1 && !pv) return alpha;
		}
	}
skip_hash_move:
	gen_captures<me, 0>(Current->moves);
	Current->current = Current->moves;
	while (move = pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (F(see<me>(move,-50))) continue;
		if (SeeValue[Square(To(move))] > SeeValue[Square(From(move))]) cnt++;
		do_move<me>(move);
		value = -q_search<opp, pv>(-beta, -alpha, depth - 1, FlagNeatSearch);
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}

	if (depth < -2) goto finish;
	if (depth <= -1 && Current->score + 50 < alpha) goto finish;
	gen_checks<me>(Current->moves);
	Current->current = Current->moves;
	while (move = pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (IsRepetition(alpha + 1,move)) continue;
		if (F(see<me>(move,-50))) continue;
		do_move<me>(move);
		value = -q_evasion<opp, pv>(-beta, -alpha, depth - 1, FlagNeatSearch);
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}

	if (T(cnt) || Current->score + 30 < alpha || T(Current->threat & Piece(me)) || T((Current->xray[opp] | Current->pin[opp]) & NonPawn(opp)) 
		|| T(Pawn(opp) & Line(me, 1) & Shift(me,~PieceAll))) goto finish;
	Current->margin = alpha - Current->score + 6;
	gen_delta_moves<me>(Current->moves);
	Current->current = Current->moves;
	while (move = pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (IsRepetition(alpha + 1,move)) continue;
		if (F(see<me>(move,-50))) continue;
		cnt++;
		do_move<me>(move);
		value = -q_search<opp, pv>(-beta, -alpha, depth - 1, FlagNeatSearch);
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) {
					if (Current->killer[1] != move) {
						Current->killer[2] = Current->killer[1];
						Current->killer[1] = move;
					}
					goto cut;
				}
			}
		}
		if (cnt >= 3) break; 
	}

finish:
	if (depth >= -2 && (depth >= 0 || Current->score + 50 >= alpha)) hash_high(score, 1);
	return score;
cut:
	hash_low(move, score, 1);
	return score;
}

template <bool me, bool pv> int q_evasion(int alpha, int beta, int depth, int flags) {
	int i, value, pext, score, move, cnt, hash_move, hash_depth;
	int *p;
	GEntry * Entry;

	score = Convert((Current - Data),int) - MateValue;
	if (flags & FlagHaltCheck) halt_check;
	 
#ifdef TB1 //possible entry TB
	
#endif

	hash_move = hash_depth = 0;
	if (flags & FlagHashCheck) {
	    for (i = 0, Entry = Hash + (High32(Current->key) & hash_mask); i < 4; Entry++, i++) {
		    if (Low32(Current->key) == Entry->key) {
			    if (T(Entry->low_depth)) {
				    if (Entry->low >= beta && !pv) return Entry->low;
				    if (Entry->low_depth > hash_depth && T(Entry->move)) {
					    hash_move = Entry->move;
					    hash_depth = Entry->low_depth;
				    }
			    }
			    if (T(Entry->high_depth) && Entry->high <= alpha && !pv) return Entry->high;
				break;
		    }
	    }
	}

	if (flags & FlagCallEvaluation) evaluate();
	Current->mask = Filled;
	if (Current->score - 10 <= alpha && !pv) {
		Current->mask = Piece(opp);
		score = Current->score - 10;
		capture_margin<me>(alpha, score);
	}

	alpha = Max(score, alpha);
	pext = 0;
	gen_evasions<me>(Current->moves);
	Current->current = Current->moves;
	if (F(Current->moves[0])) return score;
	if (F(Current->moves[1])) pext = 1;
	else {
		Current->ref[0] = RefM(Current->move).check_ref[0];
		Current->ref[1] = RefM(Current->move).check_ref[1];
		mark_evasions(Current->moves);
	    if (T(hash_move) && (T(Bit(To(hash_move)) & Current->mask) || T(hash_move & 0xE000))) {
	        for (p = Current->moves; T(*p); p++) {
		        if (((*p) & 0xFFFF) == hash_move) {
				    *p |= 0x7FFF0000;
				    break;
			    }
	        }
	    }
	}
	cnt = 0;
	while (move = pick_move()) {
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (IsRepetition(alpha + 1,move)) {
			score = Max(0, score);
			continue;
		}
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (cnt > 3 && F(is_check<me>(move)) && !pv) continue;
			if ((value = Current->score + DeltaM(move) + 10) <= alpha && !pv) {
				score = Max(value, score);
				continue;
			}
		}
		do_move<me>(move);
		value = -q_search<opp, pv>(-beta, -alpha, depth - 1 + pext, FlagNeatSearch);
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}
	return score;
cut:
	return score;
}

void send_position(GPos * Pos) {
	Pos->Position->key = Current->key;
	Pos->Position->pawn_key = Current->pawn_key;
	Pos->Position->move = Current->move;
	Pos->Position->capture = Current->capture;
	Pos->Position->turn = Current->turn;
	Pos->Position->castle_flags = Current->castle_flags;
	Pos->Position->ply = Current->ply;
	Pos->Position->ep_square = Current->ep_square;
	Pos->Position->piece = Current->piece;
	Pos->Position->pst = Current->pst;
	Pos->Position->material = Current->material;
	for (int i = 0; i < 64; i++) Pos->Position->square[i] = Board->square[i];
	Pos->date = date;
	Pos->sp = sp;
	for (int i = 0; i <= Current->ply; i++) Pos->stack[i] = Stack[sp - i];
	for (int i = 0; i < Min(16, 126 - (int)(Current - Data)); i++) {
		Pos->killer[i][0] = (Current + i + 1)->killer[1];
		Pos->killer[i][1] = (Current + i + 1)->killer[2];
	}
	for (int i = Min(16, 126 - (int)(Current - Data)); i < 16; i++) Pos->killer[i][0] = Pos->killer[i][1] = 0;
}

void retrieve_board(GPos * Pos) {
	for (int i = 0; i < 16; i++) Board->bb[i] = 0;
	for (int i = 0; i < 64; i++) {
		int piece = Pos->Position->square[i];
		Board->square[i] = piece;
		if (piece) {
			Board->bb[piece & 1] |= Bit(i);
			Board->bb[piece] |= Bit(i);
		}
	}
}

void retrieve_position(GPos * Pos, int copy_stack) {
	Current->key = Pos->Position->key;
	Current->pawn_key = Pos->Position->pawn_key;
	Current->move = Pos->Position->move;
	Current->capture = Pos->Position->capture;
	Current->turn = Pos->Position->turn;
	Current->castle_flags = Pos->Position->castle_flags;
	Current->ply = Pos->Position->ply;
	Current->ep_square = Pos->Position->ep_square;
	Current->piece = Pos->Position->piece;
	Current->pst = Pos->Position->pst;
	Current->material = Pos->Position->material;
	retrieve_board(Pos);
	Current->piece_nb = popcnt(PieceAll); // TB
	date = Pos->date;
	if (copy_stack) {
		sp = Current->ply;
		for (int i = 0; i <= sp; i++) Stack[sp - i] = Pos->stack[i];
	} else sp = Pos->sp;
	for (int i = 0; i < 16; i++) {
		(Current + i + 1)->killer[1] = Pos->killer[i][0];
		(Current + i + 1)->killer[2] = Pos->killer[i][1];
	}
}

void halt_all(GSP * Sp, int locked) {
	GMove * M;
	if (!locked) LOCK(Sp->lock);
	if (Sp->active) {
		for (int i = 0; i < Sp->move_number; i++) {
			M = &Sp->move[i];
			if ((M->flags & FlagClaimed) && !(M->flags & FlagFinished) && M->id != Id) SET_BIT_64(Smpi->stop, M->id);
		}
		Sp->active = Sp->claimed = 0;
		ZERO_BIT_64(Smpi->active_sp, (int)(Sp - Smpi->Sp));
	}
	if (!locked) UNLOCK(Sp->lock);
}

void halt_all(int from, int to) {
	for (uint64 u = Smpi->active_sp; u; Cut(u)) {
		GSP * Sp = &Smpi->Sp[lsb(u)];
		LOCK(Sp->lock);
		if (Sp->height >= from && Sp->height <= to) halt_all(Sp, 1);
		UNLOCK(Sp->lock);
	}
}

void init_sp(GSP * Sp, int alpha, int beta, int depth, int pv, int singular, int height) {
	Sp->claimed = 1;
	Sp->active = Sp->finished = 0;
	Sp->best_move = 0;
	Sp->alpha = alpha;
	Sp->beta = beta;
	Sp->depth = depth;
	Sp->split = 0;
	Sp->singular = singular;
	Sp->height = height;
	Sp->move_number = 0;
	Sp->pv = pv;
}

template <bool me> int smp_search(GSP * Sp) {
	int i, value, move, alpha, iter = 0;
	if (!Sp->move_number) return Sp->alpha;
	send_position(Sp->Pos);
	if (setjmp(Sp->jump)) {
		LOCK(Sp->lock);
		halt_all(Sp, 1);
		UNLOCK(Sp->lock);
		halt_all(Sp->height + 1, 127);
		Current = Data + Sp->height;
		sp = Sp->Pos->sp;
		retrieve_board(Sp->Pos);
		return Sp->beta;
	}
	LOCK(Sp->lock);
	SET_BIT_64(Smpi->active_sp, (int)(Sp - Smpi->Sp));
	Sp->active = 1;
	Sp->claimed = Sp->finished = 0;
loop:
	for (i = 0; i < Sp->move_number; i++) {
		GMove * M = &Sp->move[i];
		if (!iter) Sp->current = i;
		if (M->flags & FlagFinished) continue;
		if (!iter) {
			if (M->flags & FlagClaimed) continue;
			M->flags |= FlagClaimed;
			M->id = Id;
		} else if (M->flags & FlagClaimed) {
			SET_BIT_64(Smpi->stop, M->id);
			M->id = Id;
		}
		move = M->move;
		alpha = Sp->alpha;
		UNLOCK(Sp->lock);
		do_move<me>(move);
		value = -search<opp, 0>(-alpha, M->reduced_depth, FlagNeatSearch | ExtFlag(M->ext));
		if (value > alpha && (Sp->pv || M->reduced_depth < M->research_depth)) {
			if (Sp->pv) value = -pv_search<opp, 0>(-Sp->beta, -Sp->alpha, M->research_depth, FlagNeatSearch | ExtFlag(M->ext));
			else value = -search<opp, 0>(-alpha, M->research_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(M->ext));
		}
		undo_move<me>(move);
		LOCK(Sp->lock);
		if (Sp->finished) goto cut;
		M->flags |= FlagFinished;
		if (value > Sp->alpha) {
			Sp->best_move = move;
			Sp->alpha = Min(value, Sp->beta);
			if (value >= Sp->beta) goto cut;
		}
	}
	if (!iter) {
		iter++;
		goto loop;
	}
	halt_all(Sp, 1);
	UNLOCK(Sp->lock);
	return Sp->alpha;
cut:
	halt_all(Sp, 1);
	UNLOCK(Sp->lock);
	return Sp->beta;
}

template <bool me, bool exclusion> int search(int beta, int depth, int flags) {
	int i, value, cnt, flag, moves_to_play, check, score, move, ext, margin, hash_move, do_split, sp_init, singular, played,
		high_depth, high_value, hash_value, new_depth, move_back, hash_depth, *p;
	int height = (int)(Current - Data);
	GSP * Sp;


	if (nodes > check_node_smp + 0x10) {
#ifdef WINDOWS_64
		InterlockedAdd64(&Smpi->nodes, (long long)(nodes)-(long long)(check_node_smp));
#else // WIN_X32
		Smpi->nodes += (long long)(nodes)-(long long)(check_node_smp);
#endif // WINDOWS_X64
		check_node_smp = nodes;
		check_state();
		if (nodes > check_node + 0x4000 && parent) {
			check_node = nodes;
			check_time(1);
			if (Searching) SET_BIT_64(Smpi->searching, Id); // BUG, don't know why this is necessary
		}
	}


	if (depth <= 1) return q_search<me, 0>(beta - 1, beta, 1, flags);
	if (flags & FlagHaltCheck) {
	    if (height - MateValue >= beta) return beta;
	    if (MateValue - height < beta) return beta - 1;
	    halt_check;
	}


#ifdef TB1 //possible entry TB
	
#endif

	if (exclusion) {
		cnt = high_depth = do_split = sp_init = singular = played = 0;
		flag = 1;
		score = beta - 1;
		high_value = MateValue; 
		hash_value = -MateValue;
		hash_depth = -1;
		hash_move = flags & 0xFFFF;
		goto skip_hash_move;
	}

	if (flags & FlagCallEvaluation) evaluate();
	if (Check(me)) return search_evasion<me, 0>(beta, depth, flags & (~(FlagHaltCheck | FlagCallEvaluation)));

	if ((value = Current->score - 90 - (depth << 3) - (Max(depth - 5, 0) << 5)) >= beta && F(Pawn(opp) & Line(me, 1) & Shift(me,~PieceAll)) && T(NonPawnKing(me)) && F(flags & (FlagReturnBestMove | FlagDisableNull)) && depth <= 13) return value;
	if ((value = Current->score + 50) < beta && depth <= 3) return MaxF(value, q_search<me, 0>(beta - 1, beta, 1, FlagHashCheck | (flags & 0xFFFF)));

	high_depth = 0;
	high_value = MateValue;
	hash_value = -MateValue;
	hash_depth = -1;
	Current->best = hash_move = flags & 0xFFFF;
	if (GEntry * Entry = probe_hash()) {
		if (Entry->high_depth > high_depth) {
			high_depth = Entry->high_depth;
			high_value = Entry->high;
		}
		if (Entry->high < beta && Entry->high_depth >= depth) return Entry->high;
		if (T(Entry->move) && Entry->low_depth > hash_depth) {
			Current->best = hash_move = Entry->move;
			hash_depth = Entry->low_depth;
			if (Entry->low_depth) hash_value = Entry->low;
		}
		if (Entry->low >= beta && Entry->low_depth >= depth) {
			if (Entry->move) {
				Current->best = Entry->move;
				if (F(Square(To(Entry->move))) && F(Entry->move & 0xE000)) {
					if (Current->killer[1] != Entry->move && F(flags & FlagNoKillerUpdate)) {
						Current->killer[2] = Current->killer[1];
						Current->killer[1] = Entry->move;
					}
					UpdateRef(Entry->move);
				}
				return Entry->low;
			}
			if (F(flags & FlagReturnBestMove)) return Entry->low;
		}
	}
	if (depth >= 20) if (GPVEntry * PVEntry = probe_pv_hash()) {
		hash_low(PVEntry->move,PVEntry->value,PVEntry->depth);
		hash_high(PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth) {
			if (PVEntry->move) Current->best = PVEntry->move;
			if (F(flags & FlagReturnBestMove) && ((Current->ply <= 50 && PVEntry->ply <= 50) || (Current->ply >= 50 && PVEntry->ply >= 50))) return PVEntry->value;
		}
		if (T(PVEntry->move) && PVEntry->depth > hash_depth) {
			Current->best = hash_move = PVEntry->move;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}
	if (depth < 10) score = height - MateValue;
	else score = beta - 1;

#ifdef TB //possible entry TB
	if (useWDL && Current->piece_nb <= Smpi->WdlPieces && depth >= (probedepth*2)) {
		int success;
		value = probe_win(&success);
		if (success) {
			if (value == 0) return 0;
			if (value > EvalValue) {
				if (value >= beta) return value;
				score = Max(score, value);
			}
			else if (value < -EvalValue && value < beta) return value;
		}
	}
#endif

	if (depth >= 12 && (F(hash_move) || hash_value < beta || hash_depth < depth - 12) && (high_value >= beta || high_depth < depth - 12) && F(flags & FlagDisableNull)) {
		new_depth = depth - 8;
		value = search<me, 0>(beta, new_depth, FlagHashCheck | FlagNoKillerUpdate | FlagDisableNull | FlagReturnBestMove | hash_move);
		if (value >= beta) {
			if (Current->best) hash_move = Current->best;
			hash_depth = new_depth;
			hash_value = beta;
		}
	}
	if (depth >= 4 && Current->score + 3 >= beta && F(flags & (FlagDisableNull | FlagReturnBestMove))
		&& (high_value >= beta || high_depth < depth - 10) && (depth < 12 || (hash_value >= beta && hash_depth >= depth - 12)) && beta > -EvalValue && T(NonPawnKing(me))) {
		new_depth = depth - 8;
		do_null();
	    value = -search<opp, 0>(1 - beta, new_depth, FlagHashCheck);
		undo_null();
		if (value >= beta) {
			if (depth < 12) hash_low(0, value, depth);
			return value;
		}
	}

	cnt = flag = singular = played = 0;
	if (T(hash_move) && is_legal<me>(move = hash_move)) {
		if (IsIllegal(me,move)) goto skip_hash_move;
		cnt++;
		check = is_check<me>(move);
		if (check) ext = 1 + (depth < 16);
		else ext = extension<0>(move, depth);
		if (depth >= 16 && hash_value >= beta && hash_depth >= (new_depth = depth - Min(12, depth/2))) {
			int margin_one = beta - ExclSingle(depth);
			int margin_two = beta - ExclDouble(depth);
			int prev_ext = Ext(flags);
			singular = singular_extension<me>(ext,prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
		}
		if (depth < 16 && To(move) == To(Current->move) && T(Square(To(move)))) ext = Max(ext, 2);
		new_depth = depth - 2 + ext;
		do_move<me>(move);
		value = -search<opp, 0>(1 - beta, new_depth, FlagNeatSearch | ((hash_value >= beta && hash_depth >= depth - 12) ? FlagDisableNull : 0) | ExtFlag(ext));
		undo_move<me>(move);
		played++;
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
skip_hash_move:
	Current->killer[0] = 0;
	Current->stage = stage_search;
	Current->gen_flags = 0;
	Current->ref[0] = RefM(Current->move).ref[0];
	Current->ref[1] = RefM(Current->move).ref[1];
	move_back = 0;
	if (beta > 0 && Current->ply >= 2) {
		if (F((Current - 1)->move & 0xF000)) {
			move_back = (To((Current - 1)->move) << 6) | From((Current - 1)->move);
			if (Square(To(move_back))) move_back = 0;
		}
	}
	moves_to_play = 3 + (depth * depth)/6;
	margin = Current->score + 70 + (depth << 3) + (Max(depth - 7, 0) << 5);
	if ((value = margin) < beta && depth <= 19) {
		flag = 1;
		score = Max(value, score);
		Current->stage = stage_razoring;
		Current->mask = Piece(opp);
		if ((value = Current->score + 200 + (depth << 5)) < beta) {
			score = Max(value, score);
			Current->mask ^= Pawn(opp);
		}
	}
	Current->current = Current->moves;
	Current->moves[0] = 0;
	if (depth <= 5) Current->gen_flags |= FlagNoBcSort;
	
	do_split = sp_init = 0;
	if (depth >= SplitDepth && PrN > 1 && parent && !exclusion) do_split = 1;

	while (move = get_move<me,0>()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (move == move_back) {
			score = Max(0, score);
			continue;
		}
		if (Current->stage == r_checks) check = 1;
		else check = is_check<me>(move);
		if (T(check) && T(see<me>(move, 0))) ext = 1 + (depth < 16);
		else ext = extension<0>(move, depth);
		new_depth = depth - 2 + ext;
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (move != Current->killer[1] && move != Current->killer[2]) {
				if (F(check) && cnt > moves_to_play) {
				    Current->gen_flags &= ~FlagSort;
				    continue;
		        }
				if (depth >= 6) {
					int reduction = msb(cnt);
					if (move == Current->ref[0] || move == Current->ref[1]) reduction = Max(0, reduction - 1);
					if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
					if (new_depth - reduction > 3)
						if (F(see<me>(move, -50))) reduction += 2;
					if (T(reduction) && reduction < 2 && new_depth - reduction > 3) {
						if (cnt > 3) reduction = 2;
						else reduction = 0;
					}
					new_depth = Max(3, new_depth - reduction);
				}
		    }
			if (F(check)) {
			    if ((value = Current->score + DeltaM(move) + 10) < beta && depth <= 3) {
				    score = Max(value, score);
				    continue;
			    }
				if (cnt > 7 && (value = margin + DeltaM(move) - 25 * msb(cnt)) < beta && depth <= 19) {
					score = Max(value, score);
					continue;
				}
			}
			if (depth <= 9 && T(NonPawnKing(me)) && F(see<me>(move,-50))) continue;
		} else {
			if (Current->stage == r_cap) {
				if (F(check) && depth <= 9 && F(see<me>(move,-50))) continue;
			} else if (Current->stage == s_bad_cap && F(check) && depth <= 5) continue;
		}
		if (do_split && played >= 1) {
			if (!sp_init) {
				sp_init = 1;
				uint64 u = ~Smpi->active_sp;
				if (!u) {
					do_split = 0;
					goto make_move;
				}
				Sp = &Smpi->Sp[lsb(u)];
				init_sp(Sp, beta - 1, beta, depth, 0, singular, height);
			}
			GMove * M = &Sp->move[Sp->move_number++];
			M->ext = ext;
			M->flags = 0;
			M->move = move;
			M->reduced_depth = new_depth;
			M->research_depth = depth - 2 + ext;
			M->stage = Current->stage;
			continue;
		}
make_move:
		do_move<me>(move);
		value = -search<opp, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value >= beta && new_depth < depth - 2 + ext) value = -search<opp, 0>(1 - beta, depth - 2 + ext, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		undo_move<me>(move);
		played++;
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
	if (do_split && sp_init) {
		value = smp_search<me>(Sp);
		if (value >= beta && Sp->best_move) {
			score = beta;
			Current->best = move = Sp->best_move;
			for (i = 0; i < Sp->move_number; i++) {
				GMove * M = &Sp->move[i];
				if ((M->flags & FlagFinished) && M->stage == s_quiet && M->move != move) HistoryBad(M->move);
			}
		}
		if (value >= beta) goto cut;
	}
	if (F(cnt) && F(flag)) {
		hash_high(0, 127);
		hash_low(0, 0, 127);
		return 0;
	}
	if (F(exclusion)) hash_high(score, depth);
	return score;
cut:
	if (exclusion) return score;
	Current->best = move;
	if (depth >= 10) score = Min(beta, score);
	hash_low(move, score, depth);
	if (F(Square(To(move))) && F(move & 0xE000)) {
		if (Current->killer[1] != move && F(flags & FlagNoKillerUpdate)) {
			Current->killer[2] = Current->killer[1];
			Current->killer[1] = move;
		}
		HistoryGood(move);
		if (move != hash_move && Current->stage == s_quiet && !sp_init) for (p = Current->start; p < (Current->current - 1); p++) HistoryBad(*p);
		UpdateRef(move);
	}
	return score;
}

template <bool me, bool exclusion> int search_evasion(int beta, int depth, int flags) {
	int i, value, score, pext, move, cnt, hash_value = -MateValue, hash_depth, hash_move, new_depth, ext, check, moves_to_play;
	int height = (int)(Current - Data);

	if (depth <= 1) return q_evasion<me, 0>(beta - 1, beta, 1, flags);
	score = height - MateValue;
	if (flags & FlagHaltCheck) {
	    if (score >= beta) return beta;
	    if (MateValue - height < beta) return beta - 1;
	    halt_check;
	}


#ifdef TB1 //possible entry TB

#endif

	hash_depth = -1;
	hash_move = flags & 0xFFFF;
	if (exclusion) {
		cnt = pext = 0;
		score = beta - 1;
		gen_evasions<me>(Current->moves);
	    if (F(Current->moves[0])) return score;
		goto skip_hash_move;
	}
	Current->best = hash_move;
	if (GEntry * Entry = probe_hash()) {
		if (Entry->high < beta && Entry->high_depth >= depth) return Entry->high;
		if (T(Entry->move) && Entry->low_depth > hash_depth) {
			Current->best = hash_move = Entry->move;
			hash_depth = Entry->low_depth;
		}
		if (Entry->low >= beta && Entry->low_depth >= depth) {
			if (Entry->move) {
				Current->best = Entry->move;
				if (F(Square(To(Entry->move))) && F(Entry->move & 0xE000)) UpdateCheckRef(Entry->move);
			}
			return Entry->low;
		}
		if (Entry->low_depth >= depth - 8 && Entry->low > hash_value) hash_value = Entry->low;
	}

	if (depth >= 20) if (GPVEntry * PVEntry  = probe_pv_hash()) {
		hash_low(PVEntry->move,PVEntry->value,PVEntry->depth);
		hash_high(PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth) {
			if (PVEntry->move) Current->best = PVEntry->move;
			return PVEntry->value;
		}
		if (T(PVEntry->move) && PVEntry->depth > hash_depth) {
			Current->best = hash_move = PVEntry->move;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}

	if (hash_depth >= depth && hash_value > -EvalValue) score = Min(beta - 1, Max(score, hash_value));
#ifdef TB1 //possible entry TB
	
#endif

	if (flags & FlagCallEvaluation) evaluate();

	Current->mask = Filled;
	if (Current->score - 10 < beta && depth <= 3) {
		Current->mask = Piece(opp);
		score = Current->score - 10;
	    capture_margin<me>(beta, score);
	}
	cnt = 0;
	pext = 0;
    gen_evasions<me>(Current->moves);
	if (F(Current->moves[0])) return score;
	if (F(Current->moves[1])) pext = 2;

	if (T(hash_move) && is_legal<me>(move = hash_move)) {
		if (IsIllegal(me,move)) goto skip_hash_move;
		cnt++;
		check = is_check<me>(move);
		if (check) ext = Max(pext, 1 + (depth < 16));
		else ext = MaxF(pext, extension<0>(move, depth));
		if (depth >= 16 && hash_value >= beta && hash_depth >= (new_depth = depth - Min(12, depth/2))) {
			int margin_one = beta - ExclSingle(depth);
			int margin_two = beta - ExclDouble(depth);
			int prev_ext = Ext(flags);
			int singular = singular_extension<me>(ext,prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
		}
		new_depth = depth - 2 + ext;
		do_move<me>(move);
		evaluate();
		if (Current->att[opp] & King(me)) {
			undo_move<me>(move);
			cnt--;
			goto skip_hash_move;
		}
		value = -search<opp, 0>(1 - beta, new_depth, FlagHaltCheck | FlagHashCheck | ((hash_value >= beta && hash_depth >= depth - 12) ? FlagDisableNull : 0) | ExtFlag(ext));
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
skip_hash_move:
	moves_to_play = 3 + ((depth * depth) / 6); 
	Current->ref[0] = RefM(Current->move).check_ref[0];
	Current->ref[1] = RefM(Current->move).check_ref[1];
	mark_evasions(Current->moves);
	Current->current = Current->moves;
	while (move = pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (IsRepetition(beta,move)) {
			score = Max(0, score);
			continue;
		}
		check = is_check<me>(move);
		if (check) ext = Max(pext, 1 + (depth < 16));
		else ext = MaxF(pext, extension<0>(move, depth));
		new_depth = depth - 2 + ext;
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (F(check)) {
				if (cnt > moves_to_play) continue;
				if ((value = Current->score + DeltaM(move) + 10) < beta && depth <= 3) {
					score = Max(value, score);
					continue;
				}
			}
			if (depth >= 6 && cnt > 3) {
				int reduction = msb(cnt);
				if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
				new_depth = Max(3, new_depth - reduction);
			}
		}
		do_move<me>(move);
		value = -search<opp, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value >= beta && new_depth < depth - 2 + ext) value = -search<opp, 0>(1 - beta, depth - 2 + ext, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
	if (F(exclusion)) hash_high(score, depth);
	return score;
cut:
	if (exclusion) return score;
	Current->best = move;
	hash_low(move, score, depth);
	if (F(Square(To(move))) && F(move & 0xE000)) UpdateCheckRef(move);
	return score;
}

template <bool me, bool root> int pv_search(int alpha, int beta, int depth, int flags) {
	int i, value, move, cnt, pext = 0, ext, check, hash_value = -MateValue, margin, do_split = 0, sp_init = 0, singular = 0, played = 0,
		new_depth, hash_move, hash_depth, old_alpha = alpha, old_best, ex_depth = 0, ex_value = 0, start_knodes = (nodes >> 10);
	GSP * Sp;
	int height = (int)(Current - Data);

	if (root) {
		depth = Max(depth, 2);
		flags |= ExtFlag(1);
		if (F(RootList[0])) return 0;
	    if (Print) {
			fprintf(stdout,"info depth %d\n",(depth/2)); 
			fflush(stdout);
		}
		int * p;
		for (p = RootList; *p; p++);
		sort_moves(RootList,p);
		for (p = RootList; *p; p++) *p &= 0xFFFF;
		SetScore(RootList[0],2);
		goto check_hash;
	}
	if (depth <= 1) return q_search<me, 1>(alpha, beta, 1, FlagNeatSearch);
	if (Convert((Current - Data),int) - MateValue >= beta) return beta;
	if (MateValue - Convert((Current - Data),int) <= alpha) return alpha;
	halt_check;

#ifdef TB //possible entry TB
	if (useDTZ && !root && parent && height == 1 && Current->piece_nb <= Smpi->DtzPieces && depth >= (probedepth*2)) 
	{			
		int success;
		value = probe_distance(&success);
		if (success) return value;
	}
#endif

check_hash:
	hash_depth = -1;
	Current->best = hash_move = 0;
    if (GPVEntry * PVEntry = probe_pv_hash()) {
		hash_low(PVEntry->move,PVEntry->value,PVEntry->depth);
		hash_high(PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth && T(PVHashing)) {
			if (PVEntry->move) Current->best = PVEntry->move;
			if ((Current->ply <= 50 && PVEntry->ply <= 50) || (Current->ply >= 50 && PVEntry->ply >= 50)) if (!PVEntry->value || !draw_in_pv<me>()) return PVEntry->value;
		}
		if (T(PVEntry->move) && PVEntry->depth > hash_depth) {
			Current->best = hash_move = PVEntry->move;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}
	if (GEntry * Entry = probe_hash()) {
		if (T(Entry->move) && Entry->low_depth > hash_depth) {
			Current->best = hash_move = Entry->move;
			hash_depth = Entry->low_depth;
			if (Entry->low_depth) hash_value = Entry->low;
		}
	}

#ifdef TB1 //possible entry TB

#endif

	if (root) {
		hash_move = RootList[0];
		hash_value = Previous;
		hash_depth = Max(0, depth - 2);
	}

	evaluate();

	if (F(root) && depth >= 6 && (F(hash_move) || hash_value <= alpha || hash_depth < depth - 8)) {
		if (F(hash_move)) new_depth = depth - 2;
		else new_depth = depth - 4;
		value = pv_search<me, 0>(alpha, beta, new_depth, hash_move);
		if (value > alpha) {
hash_move_found:
			if (Current->best) hash_move = Current->best;
		    hash_depth = new_depth;
		    hash_value = value;
			goto skip_iid;
		} else {
			i = 0;		
			new_depth = depth - 8;
iid_loop:
			margin = alpha - (8 << i);
			if (T(hash_move) && hash_depth >= Min(new_depth, depth - 8) && hash_value >= margin) goto skip_iid;
			value = search<me, 0>(margin, new_depth, FlagHashCheck | FlagNoKillerUpdate | FlagDisableNull | FlagReturnBestMove | hash_move);
			if (value >= margin) goto hash_move_found;
			i++;
			if (i < 5) goto iid_loop;
		}
	}
skip_iid:
	if (F(root) && Check(me)) {
		alpha = Max(Convert((Current - Data),int) - MateValue, alpha);
		Current->mask = Filled;
		gen_evasions<me>(Current->moves);
		if (F(Current->moves[0])) return Convert((Current - Data),int) - MateValue; 
	    if (F(Current->moves[1])) pext = 2;
	}

	cnt = 0;
	if (hash_move && is_legal<me>(move = hash_move)) {
		cnt++;
		if (root) {

		    memset(Data + 1, 0, 127 * sizeof(GData));

		    move_to_string(move,score_string);
		    if (Print) sprintf_s(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt);
		}
		check = is_check<me>(move);
		if (check) ext = 2;
		else ext = MaxF(pext, extension<1>(move, depth));
		if (depth >= 12 && hash_value > alpha && hash_depth >= (new_depth = depth - Min(12,depth/2))) {
			int margin_one = hash_value - ExclSinglePV(depth);
			int margin_two = hash_value - ExclDoublePV(depth);
			int prev_ext = Ext(flags);
			singular = singular_extension<me>(root ? 0 : ext,root ? 0 : prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) {
				ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
				if (root) CurrentSI->Singular = singular;
				ex_depth = new_depth;
				ex_value = (singular >= 2 ? margin_two : margin_one) - 1;
			}
		}
		new_depth = depth - 2 + ext;
		do_move<me>(move);
		if (PrN > 1) {
			evaluate();
			if (Current->att[opp] & King(me)) {
				undo_move<me>(move);
				cnt--;
				goto skip_hash_move;
			}
		}
		value = -pv_search<opp, 0>(-beta, -alpha, new_depth, ExtFlag(ext));
		undo_move<me>(move);
		played++;
		if (value > alpha) {
			if (root) {
				CurrentSI->FailLow = 0;
			    best_move = move;
			    best_score = value;
				hash_low(best_move,value,depth);
				if (depth >= 14 || T(Console)) send_pv(depth/2, old_alpha, beta, value);
			}
		    alpha = value;
			Current->best = move;
			if (value >= beta) goto cut;
		} else if (root) {
			CurrentSI->FailLow = 1;
			CurrentSI->FailHigh = 0;
			CurrentSI->Singular = 0;
			if (depth >= 14 || T(Console)) send_pv(depth/2, old_alpha, beta, value);
		}
	}
skip_hash_move:
	Current->gen_flags = 0;
	if (F(Check(me))) {
		Current->stage = stage_search;
		Current->ref[0] = RefM(Current->move).ref[0];
	    Current->ref[1] = RefM(Current->move).ref[1];
	} else {
		Current->stage = stage_evasion;
		Current->ref[0] = RefM(Current->move).check_ref[0];
		Current->ref[1] = RefM(Current->move).check_ref[1];
	}
	Current->killer[0] = 0;
	Current->moves[0] = 0;
	if (root) Current->current = RootList + 1;
	else Current->current = Current->moves;

	if (PrN > 1 && !root && parent && depth >= SplitDepthPV) do_split = 1;

	while (move = get_move<me,root>()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (root) {

		    memset(Data + 1, 0, 127 * sizeof(GData));

		    move_to_string(move,score_string);
		    if (Print) sprintf_s(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt);
		}
		if (IsRepetition(alpha + 1,move)) continue;
		check = is_check<me>(move);
		if (check) ext = 2;
		else ext = MaxF(pext, extension<1>(move, depth));
		new_depth = depth - 2 + ext;
		if (depth >= 6 && F(move & 0xE000) && F(Square(To(move))) && (T(root) || (move != Current->killer[1] && move != Current->killer[2]) || T(Check(me))) && cnt > 3) {
			int reduction = msb(cnt) - 1;
			if (move == Current->ref[0] || move == Current->ref[1]) reduction = Max(0, reduction - 1);
			if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
			new_depth = Max(3, new_depth - reduction);
		}
		if (do_split && played >= 1) {
			if (!sp_init) {
				sp_init = 1;
				uint64 u = ~Smpi->active_sp;
				if (!u) {
					do_split = 0;
					goto make_move;
				}
				Sp = &Smpi->Sp[lsb(u)];
				init_sp(Sp, alpha, beta, depth, 1, singular, height);
			}
			GMove * M = &Sp->move[Sp->move_number++];
			M->ext = ext;
			M->flags = 0;
			M->move = move;
			M->reduced_depth = new_depth;
			M->research_depth = depth - 2 + ext;
			M->stage = Current->stage;
			continue;
		}
make_move:
		do_move<me>(move);
		if (new_depth <= 1) value = -pv_search<opp, 0>(-beta, -alpha, new_depth, ExtFlag(ext));
		else value = -search<opp, 0>(-alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value > alpha && new_depth > 1) {
			if (root) {
			    SetScore(RootList[cnt - 1],1);
			    CurrentSI->Early = 0;
			    old_best = best_move;
			    best_move = move;
			}
			new_depth = depth - 2 + ext;
			value = -pv_search<opp, 0>(-beta, -alpha, new_depth, ExtFlag(ext));
			if (T(root) && value <= alpha) best_move = old_best;
		}
		undo_move<me>(move);
		played++;
		if (value > alpha) {
			if (root) {
				SetScore(RootList[cnt - 1],cnt + 3);
				CurrentSI->Change = 1;
				CurrentSI->FailLow = 0;
			    best_move = move;
			    best_score = value;
				hash_low(best_move,value,depth);
				if (depth >= 14 || T(Console)) send_pv(depth/2, old_alpha, beta, value);
			}
		    alpha = value;
			Current->best = move;
			if (value >= beta) goto cut;
		}
	}
	if (do_split && sp_init) {
		value = smp_search<me>(Sp);
		if (value > alpha && Sp->best_move) {
			alpha = value;
			Current->best = move = Sp->best_move;
		}
		if (value >= beta) goto cut;
	}
	if (F(cnt) && F(Check(me))) {
		hash_high(0, 127);
		hash_low(0, 0, 127);
		hash_exact(0, 0, 127, 0, 0, 0);
	    return 0;
	}
	if (F(root) || F(SearchMoves)) hash_high(alpha, depth);
	if (alpha > old_alpha) {
		hash_low(Current->best,alpha,depth); 
		if (Current->best != hash_move) ex_depth = 0;
		if (F(root) || F(SearchMoves)) hash_exact(Current->best,alpha,depth,ex_value,ex_depth,Convert(nodes >> 10,int) - start_knodes); 
	}
	return alpha;
cut:
	hash_low(move, alpha, depth);
	return alpha;
}

template <bool me> void root() {
	int i, depth, value, alpha, beta, delta, start_depth = 2, hash_depth = 0, hash_value, store_time = 0, time_est, ex_depth = 0, ex_value, prev_time = 0, knodes = 0;
	sint64 time;
	GPVEntry * PVEntry;

	date++;
	nodes = check_node = check_node_smp = 0;

	if (parent) Smpi->nodes = Smpi->tb_hits = 0; // TB

	memcpy(Data,Current,sizeof(GData));
	Current = Data;
#ifdef TB_ //possible entry TB
	if ( Current->piece_nb <= Smpi->DtzPieces) {
		int success;
		best_move = probe_distance(&success);
		char str[32];
		move_to_string(best_move, str);
		send_best_move();
		Searching = 0;
		if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);

		return;
	}
#endif
	evaluate();
	gen_root_moves<me>();
	if (PVN > 1) {
		memset(MultiPV,0,128 * sizeof(int));
		for (i = 0; MultiPV[i] = RootList[i]; i++);
	}
	best_move = RootList[0];
	if (F(best_move)) return;
	if (F(Infinite) && !RootList[1]) {
		Infinite = 1;
		value = pv_search<me, 1>(-MateValue, MateValue, 4, FlagNeatSearch);
		Infinite = 0;
		LastDepth = 256;
		send_pv(6, -MateValue, MateValue, value);
		send_best_move();
		Searching = 0;
		if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
		return;
	}

	memset(CurrentSI,0,sizeof(GSearchInfo));
	memset(BaseSI,0,sizeof(GSearchInfo));
	Previous = -MateValue;
	if (PVEntry = probe_pv_hash()) {
		if (is_legal<me>(PVEntry->move) && PVEntry->move == best_move && PVEntry->depth > hash_depth) {
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
			ex_depth = PVEntry->ex_depth;
			ex_value = PVEntry->exclusion;
			knodes = PVEntry->knodes;
		}
	}
	if (T(hash_depth) && PVN == 1) {
		Previous = best_score = hash_value;
		depth = hash_depth;
		if (PVHashing) {
	        send_pv(depth/2, -MateValue, MateValue, best_score);
		    start_depth = (depth + 2) & (~1);
		}
		if ((depth >= LastDepth - 8 || T(store_time)) && LastValue >= LastExactValue && hash_value >= LastExactValue && T(LastTime) && T(LastSpeed)) {
			time = TimeLimit1;
			if (ex_depth >= depth - Min(12, depth/2) && ex_value <= hash_value - ExclSinglePV(depth)) {
				BaseSI->Early = 1;
				BaseSI->Singular = 1;
				if (ex_value <= hash_value - ExclDoublePV(depth)) {
					time = (time * TimeSingTwoMargin)/100;
					BaseSI->Singular = 2;
				}
				else time = (time * TimeSingOneMargin)/100;
			}
			time_est = Min(LastTime,(knodes << 10)/LastSpeed);
			time_est = Max(time_est, store_time);
set_prev_time:
			LastTime = prev_time = time_est;
			if (prev_time >= time && F(Infinite)) {
				InstCnt++;
				if (time_est <= store_time) InstCnt = 0;
				if (InstCnt > 2) {
					if (T(store_time) && store_time < time_est) {
						time_est = store_time;
						goto set_prev_time;
					}
					LastSpeed = 0;
					LastTime = 0;
					prev_time = 0;
					goto set_jump;
				}
				if (hash_value > 0 && Current->ply >= 2 && F(Square(To(best_move))) && F(best_move & 0xF000) && PrevMove == ((To(best_move) << 6) | From(best_move))) goto set_jump;
				do_move<me>(best_move);
				if (Current->ply >= 100) {
					undo_move<me>(best_move);
					goto set_jump;
				}
				for (i = 4; i <= Current->ply; i+=2) if (Stack[sp-i] == Current->key) {
					undo_move<me>(best_move);
					goto set_jump;
				}
				undo_move<me>(best_move);
				LastDepth = depth;
				LastTime = prev_time;
				LastValue = LastExactValue = hash_value;
				send_best_move();
				Searching = 0;
				if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
				return;
			} else goto set_jump;
		}
	}
	LastTime = 0;
set_jump:
	memcpy(SaveBoard,Board,sizeof(GBoard));
	memcpy(SaveData,Data,sizeof(GData));
	save_sp = sp;
	if (setjmp(Jump)) {
		Current = Data;
		Searching = 0;
		if (MaxPrN > 1) {
			halt_all(0, 127);
			ZERO_BIT_64(Smpi->searching, 0);
		}
		memcpy(Board,SaveBoard,sizeof(GBoard));
		memcpy(Data,SaveData,sizeof(GData));
		sp = save_sp;
		send_best_move();
		return;
	}
	for (depth = start_depth; depth < DepthLimit; depth += 2) {

		memset(Data + 1, 0, 127 * sizeof(GData));

		CurrentSI->Early = 1;
		CurrentSI->Change = CurrentSI->FailHigh = CurrentSI->FailLow = CurrentSI->Singular = 0;
		if (PVN > 1) value = multipv<me>(depth);
		else if ((depth/2) < 7 || F(Aspiration)) LastValue = LastExactValue = value = pv_search<me, 1>(-MateValue, MateValue, depth, FlagNeatSearch);
		else {
			delta = 8;
			alpha = Previous - delta;
			beta = Previous + delta;
loop:
			if (delta >= 16 * 32) {
				LastValue = LastExactValue = value = pv_search<me, 1>(-MateValue, MateValue, depth, FlagNeatSearch);
				goto finish;
			}
			value = pv_search<me, 1>(alpha, beta, depth, FlagNeatSearch);
			if (value <= alpha) {
				CurrentSI->FailHigh = 0;
				CurrentSI->FailLow = 1;
				alpha -= delta;
				delta *= 2;
				LastValue = value;
				memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
				goto loop;
			} else if (value >= beta) {
				CurrentSI->FailHigh = 1;
				CurrentSI->FailLow = 0;
				CurrentSI->Early = 1;
				CurrentSI->Change = 0;
				CurrentSI->Singular = Max(CurrentSI->Singular, 1);
				beta += delta;
				delta *= 2;
				LastDepth = depth;
				LastTime = MaxF(prev_time,get_time() - StartTime);
				LastSpeed = nodes/Max(LastTime, 1);
				if (depth + 2 < DepthLimit) depth += 2;
				InstCnt = 0;
#ifdef TIMING
				if (depth >= 6)
#endif
				check_time(LastTime,0);

				memset(Data + 1, 0, 127 * sizeof(GData));

				LastValue = value;
				memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
				goto loop;
			} else LastValue = LastExactValue = value;
		}
finish:
		if (value < Previous - 50) CurrentSI->Bad = 1;
		else CurrentSI->Bad = 0;
		memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
		LastDepth = depth;
		LastTime = MaxF(prev_time,get_time() - StartTime);
		LastSpeed = nodes/Max(LastTime, 1);
		Previous = value;
		InstCnt = 0;
#ifdef TIMING
		if (depth >= 6)
#endif
		check_time(LastTime,0);
	}
	Searching = 0;
	if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
	if (F(Infinite) || DepthLimit < 256) send_best_move();
}

template <bool me> int multipv(int depth) {
	int move, low = MateValue, value, i, cnt, ext, new_depth = depth;
	fprintf(stdout,"info depth %d\n",(depth/2)); fflush(stdout);
	for (cnt = 0; cnt < PVN && T(move = (MultiPV[cnt] & 0xFFFF)); cnt++) {
		MultiPV[cnt] = move;
		move_to_string(move,score_string);
		if (T(Print)) sprintf_s(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt + 1);
		new_depth = depth - 2 + (ext = extension<1>(move, depth));
		do_move<me>(move);
		value = -pv_search<opp, 0>(-MateValue,MateValue,new_depth,ExtFlag(ext));
		MultiPV[cnt] |= value << 16;
		if (value < low) low = value;
		undo_move<me>(move);
		for (i = cnt - 1; i >= 0; i--) {
			if ((MultiPV[i] >> 16) < value) {
				MultiPV[i + 1] = MultiPV[i];
				MultiPV[i] = move | (value << 16);
			}
		}
		best_move = MultiPV[0] & 0xFFFF;
		Current->score = MultiPV[0] >> 16;
		send_multipv((depth/2), cnt);
	}
	for (;T(move = (MultiPV[cnt] & 0xFFFF)); cnt++) {
		MultiPV[cnt] = move;
		move_to_string(move,score_string);
		if (T(Print)) sprintf_s(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt + 1);
		new_depth = depth - 2 + (ext = extension<1>(move, depth));
		do_move<me>(move);
		value = -search<opp, 0>(-low, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value > low) value = -pv_search<opp, 0>(-MateValue,-low,new_depth,ExtFlag(ext));
		MultiPV[cnt] |= value << 16;
		undo_move<me>(move);
		if (value > low) {
			for (i = cnt; i >= PVN; i--) MultiPV[i] = MultiPV[i - 1];
			MultiPV[PVN - 1] = move | (value << 16);
			for (i = PVN - 2; i >= 0; i--) {
				if ((MultiPV[i] >> 16) < value) {
					MultiPV[i + 1] = MultiPV[i];
					MultiPV[i] = move | (value << 16);
				}
			}
			best_move = MultiPV[0] & 0xFFFF;
		    Current->score = MultiPV[0] >> 16;
			low = MultiPV[PVN - 1] >> 16;
			send_multipv((depth/2), cnt);
		}
	}
	return Current->score;
}

void send_pv(int depth, int alpha, int beta, int score) {
	int i, pos, move, mate = 0, mate_score, sel_depth;
	sint64 nps, snodes;
	if (F(Print)) return;
	for (sel_depth = 1; sel_depth < 127 && T((Data + sel_depth)->att[0]); sel_depth++);
	sel_depth--;
	pv_length = 64;
	if (F(move = best_move)) move = RootList[0];
	if (F(move)) return;
	PV[0] = move;
	if (Current->turn) do_move<1>(move);
	else do_move<0>(move);
	pvp = 1;
	pick_pv();
	if (Current->turn ^ 1) undo_move<1>(move);
	else undo_move<0>(move);
	pos = 0;
	for (i = 0; i < 64 && T(PV[i]); i++) {
		if (pos > 0) { 
			pv_string[pos] = ' '; 
			pos++; 
		}
        move = PV[i];
        pv_string[pos++] = ((move >> 6) & 7) + 'a';
        pv_string[pos++] = ((move >> 9) & 7) + '1';
        pv_string[pos++] = (move & 7) + 'a';
        pv_string[pos++] = ((move >> 3) & 7) + '1';
        if (IsPromotion(move)) {
            if ((move & 0xF000) == FlagPQueen)  pv_string[pos++] = 'q';
            else if ((move & 0xF000) == FlagPRook)   pv_string[pos++] = 'r';
            else if ((move & 0xF000) == FlagPLight || (move & 0xF000) == FlagPDark) pv_string[pos++] = 'b';
            else if ((move & 0xF000) == FlagPKnight) pv_string[pos++] = 'n';
		}
        pv_string[pos] = 0;
	}
	score_string[0] = 'c';
	score_string[1] = 'p';
    if (score > EvalValue) { // if (score > MateScore) { TB
		mate = 1;
		strcpy(score_string,"mate ");
		mate_score = (MateValue - score + 1)/2;
	    score_string[6] = 0;
	} else if (score < -EvalValue) { // } else if (score < -MateScore) { TB
		mate = 1;
        strcpy(score_string,"mate ");
		mate_score = -(score + MateValue + 1)/2;
		score_string[6] = 0;
	} else {
        score_string[0] = 'c';
	    score_string[1] = 'p';
		score_string[2] = ' ';
		score_string[3] = 0;
	}
	nps = get_time() - StartTime;
#ifdef MP_NPS
	snodes = Smpi->nodes;
#else
	snodes = nodes;
#endif
	if (nps) nps = (snodes * 1000)/nps; 
	if (score < beta) {
		if (score <= alpha) fprintf(stdout, "info depth %d seldepth %d score %s%d upperbound nodes %I64d nps %I64d tbhits %I64d pv %s\n", depth, sel_depth, score_string, (mate ? mate_score : score), snodes, nps, Smpi->tb_hits, pv_string);
		else fprintf(stdout, "info depth %d seldepth %d score %s%d nodes %I64d nps %I64d tbhits %I64d pv %s\n", depth, sel_depth, score_string, (mate ? mate_score : score), snodes, nps, Smpi->tb_hits, pv_string);
	}
	else fprintf(stdout, "info depth %d seldepth %d score %s%d lowerbound nodes %I64d nps %I64d tbhits %I64d pv %s\n", depth, sel_depth, score_string, (mate ? mate_score : score), snodes, nps, Smpi->tb_hits, pv_string);
	fflush(stdout);
}

void send_multipv(int depth, int curr_number) {
	int i, j, pos, move, score;
	sint64 nps, snodes;
	if (F(Print)) return;
	for (j = 0; j < PVN && T(MultiPV[j]); j++) {
		pv_length = 63;
		pvp = 0;
		move = MultiPV[j] & 0xFFFF;
		score = MultiPV[j] >> 16;
		memset(PV,0,64 * sizeof(uint16));
		if (Current->turn) do_move<1>(move);
	    else do_move<0>(move);
		pick_pv();
		if (Current->turn ^ 1) undo_move<1>(move);
	    else undo_move<0>(move);
		for (i = 63; i > 0; i--) PV[i] = PV[i - 1];
		PV[0] = move;
		pos = 0;
		for (i = 0; i < 64 && T(PV[i]); i++) {
			if (pos > 0) { 
				pv_string[pos] = ' '; 
				pos++; 
			}
        	move = PV[i];
        	pv_string[pos++] = ((move >> 6) & 7) + 'a';
        	pv_string[pos++] = ((move >> 9) & 7) + '1';
        	pv_string[pos++] = (move & 7) + 'a';
        	pv_string[pos++] = ((move >> 3) & 7) + '1';
        	if (IsPromotion(move)) {
            	if ((move & 0xF000) == FlagPQueen)  pv_string[pos++] = 'q';
            	else if ((move & 0xF000) == FlagPRook)   pv_string[pos++] = 'r';
            	else if ((move & 0xF000) == FlagPLight || (move & 0xF000) == FlagPDark) pv_string[pos++] = 'b';
            	else if ((move & 0xF000) == FlagPKnight) pv_string[pos++] = 'n';
			}
        	pv_string[pos] = 0;
		}
		score_string[0] = 'c';
		score_string[1] = 'p';
		if (score > EvalValue) { //if (score > MateScore) { TB
			strcpy(score_string,"mate ");
			score = (MateValue - score + 1)/2;
	    	score_string[6] = 0;
		} else if (score < -EvalValue) {// } else if (score < -MateScore) { TB
        	strcpy(score_string,"mate ");
			score = -(score + MateValue + 1)/2;
			score_string[6] = 0;
		} else {
        	score_string[0] = 'c';
	    	score_string[1] = 'p';
			score_string[2] = ' ';
			score_string[3] = 0;
		}
		nps = get_time() - StartTime;
#ifdef MP_NPS
		snodes = Smpi->nodes;
#else
		snodes = nodes;
#endif
		if (nps) nps = (snodes * 1000) / nps;
		fprintf(stdout, "info multipv %d depth %d score %s%d nodes %I64d nps %I64d tbhits %I64d pv %s\n", j + 1, (j <= curr_number ? depth : depth - 1), score_string, score, snodes, nps, Smpi->tb_hits, pv_string);
		fflush(stdout);
	}
}

void send_best_move() {
	uint64 snodes;
	int ponder;
#ifdef CPU_TIMING
	GlobalTime[GlobalTurn] -= Convert(get_time() - StartTime, int) - GlobalInc[GlobalTurn];
	if (GlobalTime[GlobalTurn] < GlobalInc[GlobalTurn]) GlobalTime[GlobalTurn] = GlobalInc[GlobalTurn];
#endif
	if (F(Print)) return;
#ifdef MP_NPS
	snodes = Smpi->nodes;
#else
	snodes = nodes;
#endif
	fprintf(stdout,"info nodes %I64d score cp %d\n",snodes,best_score);
	if (!best_move) return;
	Current = Data;
	evaluate();
	if (Current->turn) do_move<1>(best_move);
	else do_move<0>(best_move);
	pv_length = 1;
	pvp = 0;
	pick_pv();
	ponder = PV[0];
	if (Current->turn ^ 1) undo_move<1>(best_move);
	else undo_move<0>(best_move);
	move_to_string(best_move,pv_string);
	if (ponder) {
		move_to_string(ponder,score_string);
		fprintf(stdout,"bestmove %s ponder %s\n",pv_string,score_string);
	} else fprintf(stdout,"bestmove %s\n",pv_string);
	fflush(stdout);
}

void get_position(char string[]) {
	const char * fen;
    char * moves;
    const char * ptr;
    int move, move1 = 0;

    fen = strstr(string,"fen ");
    moves = strstr(string,"moves ");
    if (fen != NULL) get_board(fen+4);
    else get_board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
	PrevMove = 0;
    if (moves != NULL) {
        ptr = moves+6;
        while (*ptr != 0) {
            pv_string[0] = *ptr++;
            pv_string[1] = *ptr++;
            pv_string[2] = *ptr++;
            pv_string[3] = *ptr++;
            if (*ptr == 0 || *ptr == ' ') pv_string[4] = 0;
            else { 
				pv_string[4] = *ptr++; 
				pv_string[5] = 0; 
			}
			evaluate();
            move = move_from_string(pv_string);
			PrevMove = move1;
			move1 = move;
            if (Current->turn) do_move<1>(move);
	        else do_move<0>(move);
			memcpy(Data,Current,sizeof(GData));
			Current = Data;
            while (*ptr == ' ') ptr++;
        }
    }
	memcpy(Stack, Stack + sp - Current->ply, (Current->ply + 1) * sizeof(uint64));
	sp = Current->ply;
}

void get_time_limit(char string[]) {
	const char * ptr;
	int i, time, inc, wtime, btime, winc, binc, moves, pondering, movetime = 0, exp_moves = MovesTg - 1;
  
	Infinite = 1;
	MoveTime = 0;
	SearchMoves = 0;
	SMPointer = 0;
	pondering = 0;
	TimeLimit1 = 0;
	TimeLimit2 = 0;
	wtime = btime = 0;
	winc = binc = 0;
	moves = 0;
	Stop = 0;
	DepthLimit = 256;
    ptr = strtok(string," ");
    for (ptr = strtok(NULL," "); ptr != NULL; ptr = strtok(NULL," ")) {
		if (!strcmp(ptr,"binc")) {
			ptr = strtok(NULL," "); 
			binc = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr,"btime")) { 
			ptr = strtok(NULL," "); 
			btime = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr,"depth")) { 
			ptr = strtok(NULL," "); 
			DepthLimit = 2 * atoi(ptr) + 2; 
			Infinite = 1;
		} else if (!strcmp(ptr,"infinite")) { 
			Infinite = 1; 
		} else if (!strcmp(ptr,"movestogo")) { 
			ptr = strtok(NULL," "); 
			moves = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr,"winc")) { 
			ptr = strtok(NULL," "); 
			winc = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr,"wtime")) { 
			ptr = strtok(NULL," "); 
			wtime = atoi(ptr); 
			Infinite = 0;
		} else if (!strcmp(ptr,"movetime")) { 
			ptr = strtok(NULL," "); 
			movetime = atoi(ptr);
			MoveTime = 1;
			Infinite = 0;
		} else if (!strcmp(ptr,"searchmoves")) {
			if (F(SearchMoves)) {
				for (i = 0; i < 256; i++) SMoves[i] = 0;
			}
		    SearchMoves = 1;
		    ptr += 12;
			while (ptr != NULL && ptr[0] >= 'a' && ptr[0] <= 'h' && ptr[1] >= '1' && ptr[1] <= '8') {
				pv_string[0] = *ptr++;
                pv_string[1] = *ptr++;
                pv_string[2] = *ptr++;
                pv_string[3] = *ptr++;
                if (*ptr == 0 || *ptr == ' ') pv_string[4] = 0;
                else { 
				    pv_string[4] = *ptr++; 
				    pv_string[5] = 0; 
			    }
				SMoves[SMPointer] = move_from_string(pv_string);
				SMPointer++;
				ptr = strtok(NULL," ");
			}
		} else if (!strcmp(ptr,"ponder")) pondering = 1;
    }
	if (pondering) Infinite = 1;
	if (Current->turn == White) {
		time = wtime;
		inc = winc;
	} else {
		time = btime;
		inc = binc;
	}
#ifdef CPU_TIMING
	if (CpuTiming) {
		time = GlobalTime[GlobalTurn];
		inc = GlobalInc[GlobalTurn];
		if (UciMaxDepth) DepthLimit = 2 * UciMaxDepth + 2;
	}
#endif
	if (moves) moves = Max(moves - 1, 1);
	int time_max = Max(time - Min(1000, time/2), 0);
	int nmoves;
	if (moves) nmoves = moves;
	else {
		nmoves = MovesTg - 1;
		if (Current->ply > 40) nmoves += Min(Current->ply - 40, (100 - Current->ply)/2);
		exp_moves = nmoves;
	}
	TimeLimit1 = Min(time_max, (time_max + (Min(exp_moves, nmoves) * inc))/Min(exp_moves, nmoves));
	TimeLimit2 = Min(time_max, (time_max + (Min(exp_moves, nmoves) * inc))/Min(3,Min(exp_moves, nmoves)));
	TimeLimit1 = Min(time_max, (TimeLimit1 * TimeRatio)/100);
	if (Ponder) TimeLimit1 = (TimeLimit1 * PonderRatio)/100;
	if (MoveTime) {
		TimeLimit2 = movetime;
		TimeLimit1 = TimeLimit2 * 100;
	}
    InfoTime = StartTime = get_time();
	Searching = 1;
	if (MaxPrN > 1) SET_BIT_64(Smpi->searching, 0);
	if (F(Infinite)) PVN = 1;
	if (Current->turn == White) root<0>(); else root<1>();
}

sint64 get_time() {
#ifdef CPU_TIMING
#ifndef TIMING
	if (CpuTiming) {
#endif
		uint64 ctime;
		QueryProcessCycleTime(GetCurrentProcess(), &ctime);
#ifdef TIMING
		return ctime / (CyclesPerMSec / 1000);
#endif
		return (ctime / CyclesPerMSec);
#ifndef TIMING
	}
#endif
#endif
	return GetTickCount();
}

int time_to_stop(GSearchInfo * SI, int time, int searching) {
	if (Infinite) return 0;
	if (time > TimeLimit2) return 1;
	if (searching) return 0;
	if (2 * time > TimeLimit2 && F(MoveTime)) return 1;
	if (SI->Bad) return 0;
	if (time > TimeLimit1) return 1;
	if (T(SI->Change) || T(SI->FailLow)) return 0;
	if (time * 100 > TimeLimit1 * TimeNoChangeMargin) return 1;
	if (F(SI->Early)) return 0;
	if (time * 100 > TimeLimit1 * TimeNoPVSCOMargin) return 1;
	if (SI->Singular < 1) return 0;
	if (time * 100 > TimeLimit1 * TimeSingOneMargin) return 1;
	if (SI->Singular < 2) return 0;
	if (time * 100 > TimeLimit1 * TimeSingTwoMargin) return 1;
	return 0;
}

void check_time(int searching) {
#ifdef CPU_TIMING
	if (CpuTiming && UciMaxKNodes && nodes > UciMaxKNodes * 1024) Stop = 1;
#endif

	while (!Stop && input()) uci();

	if (Stop) goto jump;
	CurrTime = get_time();
	int Time = Convert(CurrTime - StartTime,int);
	if (T(Print) && Time > InfoLag && CurrTime - InfoTime > InfoDelay) {
		InfoTime = CurrTime;
		if (info_string[0]) {
			fprintf(stdout,"%s",info_string);
			info_string[0] = 0;
			fflush(stdout);
		}
	}
	if (time_to_stop(CurrentSI, Time, searching)) goto jump;
	return;
jump:
	Stop = 1;
	longjmp(Jump,1);
}

void check_time(int time, int searching) {

	while (!Stop && input()) uci();

	if (Stop) goto jump;
	CurrTime = get_time();
	int Time = Convert(CurrTime - StartTime,int);
	if (T(Print) && Time > InfoLag && CurrTime - InfoTime > InfoDelay) {
		InfoTime = CurrTime;
		if (info_string[0]) {
			fprintf(stdout,"%s",info_string);
			info_string[0] = 0;
			fflush(stdout);
		}
	}
	if (time_to_stop(CurrentSI, time, searching)) goto jump;
	return;
jump:
	Stop = 1;
	longjmp(Jump,1);
}

void check_state() {
	GSP *Sp, *Spc;
	int n, nc, score, best, pv, alpha, beta, new_depth, r_depth, ext, move, value;
	GMove * M;

	if (parent) {
		for (uint64 u = TEST_RESET(Smpi->fail_high); u; Cut(u)) {
			Sp = &Smpi->Sp[lsb(u)];
			LOCK(Sp->lock);
			if (Sp->active && Sp->finished) {
				UNLOCK(Sp->lock);
				longjmp(Sp->jump, 1);
			}
			UNLOCK(Sp->lock);
		}
		return;
	}

#ifdef TB //Init TB path
	if (child) if (TEST_RESET_BIT(Smpi->tb_reload, Id)) init(Smpi->SyzygyPath);

#endif

start:
	if (TEST_RESET_BIT(Smpi->stop, Id)) longjmp(CheckJump, 1);
	if (Smpi->searching & Bit(Id)) return;
	if (!(Smpi->searching & 1)) {
		Sleep(1);
		return;
	}
	while ((Smpi->searching & 1) && !Smpi->active_sp) _mm_pause();
	while ((Smpi->searching & 1) && !(Smpi->searching & Bit(Id - 1))) _mm_pause();

	Sp = NULL; best = -0x7FFFFFFF;
	for (uint64 u = Smpi->active_sp; u; Cut(u)) {
		Spc = &Smpi->Sp[lsb(u)];
		if (!Spc->active || Spc->finished || Spc->lock) continue;
		for (nc = Spc->current + 1; nc < Spc->move_number; nc++) if (!(Spc->move[nc].flags & FlagClaimed)) break;
		if (nc < Spc->move_number) score = 1024 * 1024 + 512 * 1024 * (Spc->depth >= 20) + 128 * 1024 * (!(Spc->split))
			+ ((Spc->depth + 2 * Spc->singular) * 1024) - (((16 * 1024) * (nc - Spc->current)) / nc);
		else continue;
		if (score > best) {
			best = score;
			Sp = Spc;
			n = nc;
		}
	}

	if (Sp == NULL) goto start;
	if (!Sp->active || Sp->finished || (Sp->move[n].flags & FlagClaimed) || n <= Sp->current || n >= Sp->move_number) goto start;
	if (Sp->lock) goto start;

	LOCK(Sp->lock);
	if (!Sp->active || Sp->finished || (Sp->move[n].flags & FlagClaimed) || n <= Sp->current || n >= Sp->move_number) {
		UNLOCK(Sp->lock);
		goto start;
	}

	M = &Sp->move[n];
	M->flags |= FlagClaimed;
	M->id = Id;
	Sp->split |= Bit(Id);
	pv = Sp->pv;
	alpha = Sp->alpha;
	beta = Sp->beta;
	new_depth = M->reduced_depth;
	r_depth = M->research_depth;
	ext = M->ext;
	move = M->move;

	Current = Data;
	retrieve_position(Sp->Pos, 1);
	evaluate();
	SET_BIT_64(Smpi->searching, Id);
	UNLOCK(Sp->lock);

	if (setjmp(CheckJump)) {
		ZERO_BIT_64(Smpi->searching, Id);
		return;
	}
	if (Current->turn == White) {
		do_move<0>(move);
		if (pv) {
			value = -search<1, 0>(-alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value > alpha) value = -pv_search<1, 0>(-beta, -alpha, r_depth, ExtFlag(ext));
		} else {
			value = -search<1, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value >= beta && new_depth < r_depth) value = -search<1, 0>(1 - beta, r_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		}
		undo_move<0>(move);
	} else {
		do_move<1>(move);
		if (pv) {
			value = -search<0, 0>(-alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value > alpha) value = -pv_search<0, 0>(-beta, -alpha, r_depth, ExtFlag(ext));
		} else {
			value = -search<0, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value >= beta && new_depth < r_depth) value = -search<0, 0>(1 - beta, r_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		}
		undo_move<1>(move);
	}

	LOCK(Sp->lock);
	ZERO_BIT_64(Smpi->searching, Id);
	if (TEST_RESET_BIT(Smpi->stop, Id)) {
		UNLOCK(Sp->lock);
		return;
	}
	M->flags |= FlagFinished;
	if (value > Sp->alpha) {
		Sp->alpha = Min(value, beta);
		Sp->best_move = move;
		if (value >= beta) {
			Sp->finished = 1;
			SET_BIT_64(Smpi->fail_high, (int)(Sp - Smpi->Sp));
		}
	}
	UNLOCK(Sp->lock);
}

int input() {
	if (child) return 0;
    DWORD p;
	if (F(Input)) return 0;
	if (F(Console)) {
	    if (PeekNamedPipe(StreamHandle,NULL,0,NULL,&p,NULL)) return (p > 0);
        else return 1;
	} else return 0;
}

void epd_test(char string[], int time_limit) {
	int n = 0, positions = 4000;
	uint64 Time, all_nodes = 0, new_time, total_time;
	double prod = 0.0;
	char * ptr;
	FILE * f = fopen(string, "r");
	if (f == NULL) {
		fprintf(stdout, "File not found\n");
		return;
	}
	Infinite = 1;
	Time = get_time();
	int total_depth = 0;
	Print = 0;
	Input = 0;
	total_time = 1;
	while (!feof(f) && n < positions) {
	new_position:
		(void)fgets(mstring, 65536, f);
		ptr = strchr(mstring, '\n');
		if (ptr != NULL) *ptr = 0;
		get_board(mstring);
		evaluate();
		if (Current->turn == White) {
			gen_root_moves<0>();
		} else {
			gen_root_moves<1>();
		}
		Infinite = 0;
		MoveTime = TimeLimit1 = 100000000;
#ifndef TIME_TO_DEPTH
		TimeLimit2 = time_limit;
#else
		TimeLimit2 = TimeLimit1;
#endif
		DepthLimit = 255;
		n++;
		Stop = 0;
		Smpi->nodes = nodes = check_node = check_node_smp = 0;
		StartTime = get_time();
		if (setjmp(Jump)) {
			halt_all(0, 127);
		stop_searching:
			ZERO_BIT_64(Smpi->searching, Id);
			Searching = 0;
			Current = Data;
			new_time = Max(get_time() - StartTime, 1);
			total_time += new_time;
#ifdef MP_NPS
			all_nodes += Smpi->nodes;
#else
			all_nodes += nodes;
#endif
			total_depth += LastDepth / 2;
#ifndef TIME_TO_DEPTH
			fprintf(stdout, "Position %d: %d [%lf, %d]\n", n, LastDepth / 2, ((double)total_depth) / ((double)n), (all_nodes * Convert(1000, uint64)) / total_time);
#else
			prod += log((double)new_time);
			fprintf(stdout, "Position %d: %d [%.0lf, %d]\n", n, new_time, exp(prod / (double)n), (all_nodes * Convert(1000, uint64)) / total_time);
#endif
			goto new_position;
		}
		for (int d = 4; d < 128; d += 2) {
			LastDepth = d;
			Searching = 1;
			SET_BIT_64(Smpi->searching, Id);
			if (Current->turn == White) {
				pv_search<0, 1>(-MateValue, MateValue, d, FlagNeatSearch);
			} else {
				pv_search<1, 1>(-MateValue, MateValue, d, FlagNeatSearch);
			}
#ifdef TIME_TO_DEPTH
			if (d >= (time_limit * 2)) goto stop_searching;
#endif
		}
	}
	if (n == 0) {
		fprintf(stdout, "Empty file\n");
		return;
	}
	fclose(f);
}

void uci() {
    char *ptr = NULL;
	int i;
	sint64 value;

    (void)fgets(mstring, 65536, stdin);
    if (feof(stdin)) exit(0);
    ptr = strchr(mstring, '\n');
    if (ptr != NULL) *ptr = 0;
    if (!strcmp(mstring, "uci")) {

		fprintf(stdout, "id name %s%s%s\n", ENGINE, VERSION_, PLATFORM_);
		fprintf(stdout,"id author ThinkingALot\n");
#ifdef WINDOWS_X64
		fprintf(stdout,"option name Hash type spin min 1 max 65536 default 16\n");
#else  // WIN_X32
		fprintf(stdout,"option name Hash type spin min 1 max 1024 default 16\n");
#endif  // WINDOWS_X64
		fprintf(stdout,"option name Ponder type check default false\n");
		fprintf(stdout,"option name MultiPV type spin min 1 max 64 default 1\n");
		fprintf(stdout,"option name Clear Hash type button\n");
		fprintf(stdout,"option name PV Hash type check default true\n");
		fprintf(stdout,"option name Aspiration window type check default true\n");
#ifdef CPU_TIMING
		fprintf(stdout, "option name CPUTiming type check default false\n");
		fprintf(stdout, "option name MaxDepth type spin min 0 max 128 default 0\n");
		fprintf(stdout, "option name MaxKNodes type spin min 0 max 65536 default 0\n");
		fprintf(stdout, "option name BaseTime type spin min 0 max 1000000 default 1000\n");
		fprintf(stdout, "option name IncTime type spin min 0 max 1000000 default 5\n");
#endif
		fprintf(stdout, "option name Threads type spin min 1 max %d default %d\n", Min(CPUs, MaxPrN), PrN);
#ifdef LARGE_PAGES
		fprintf(stdout, "option name Large memory pages type check default true\n");
#endif
#ifdef TB //Options UCI
		fprintf(stdout, "option name SyzygyPath type string default <empty>\n");

		fprintf(stdout, "option name useDTZ type check default false\n");
		fprintf(stdout, "option name useWDL type check default false\n");
		fprintf(stdout, "option name SyzygyProbeDepth type spin min 1 max 100 default 1\n");
		fprintf(stdout, "option name WdlPieces type spin min 0 max 6 default 0\n");
		fprintf(stdout, "option name DtzPieces type spin min 0 max 6 default 0\n");
#endif
        fprintf(stdout,"uciok\n");
		if (F(Searching)) init_search(1);
    } else if (!strcmp(mstring,"ucinewgame")) {
        Stop = 0;
		init_search(1);
    } else if (!strcmp(mstring,"isready")) {
        fprintf(stdout,"readyok\n");
		fflush(stdout);
    }  else if (!memcmp(mstring,"position",8)) {
        if (F(Searching)) get_position(mstring);
    } else if (!memcmp(mstring,"go",2)) {
        if (F(Searching)) get_time_limit(mstring);
    } else if (!memcmp(mstring,"setoption",9)) {
		ptr = strtok(mstring," ");
		for (ptr = strtok(NULL," "); ptr != NULL; ptr = strtok(NULL," ")) {
			if (!memcmp(ptr,"Hash",4) && !Searching) {
				ptr += 11;
				value = atoi(ptr);
				if (value < 1) value = 1;
#ifdef WINDOWS_X64
				if (value > 65536) value = 65536;
#else // WIN_X32
				if (value > 1024) value = 1024;
#endif //  WINDOWS_X64
				value = (Bit(msb(value)) * Convert(1024 * 1024, sint64)) / Convert(sizeof(GEntry), sint64);
				if (value != hash_size) {
					ResetHash = 1;
					hash_size = value;
					longjmp(ResetJump, 1);
				}
			} else if (!memcmp(ptr, "Threads", 7) && !Searching) {
				ptr += 14;
				value = atoi(ptr);
				if (value != PrN) {
					NewPrN = Max(1, Min(MaxPrN, value));
					ResetHash = 0;
					longjmp(ResetJump, 1);
				}
			} 
#ifdef TB //Options UCI
			else if (!memcmp(ptr, "SyzygyPath", 10)) {
				ptr += 17;
				strcpy(Smpi->SyzygyPath, ptr);
				init(ptr);
				for (i = 1; i < MaxPrN; i++) SET_BIT_64(Smpi->tb_reload, i);
			}
	
			else if (!memcmp(ptr, "useDTZ", 6)) {
		       ptr += 13;
		       if (ptr[0] == 't') useDTZ = 1;
		       else useDTZ = 0;
			}
			else if (!memcmp(ptr, "useWDL", 6)) {
				ptr += 13;
				if (ptr[0] == 't') useWDL = 1;
				else useWDL = 0;
			}
			else if (!memcmp(ptr, "SyzygyProbeDepth", 16)) {
				ptr += 23;
				probedepth = atoi(ptr);
				
			}
			else if (!memcmp(ptr, "WdlPieces", 9)) {
				ptr += 16;
				Smpi->WdlPieces = atoi(ptr);
			}
			else if (!memcmp(ptr, "DtzPieces", 9)) {
				ptr += 16;
				Smpi->DtzPieces = atoi(ptr);
			}
#endif
			else if (!memcmp(ptr, "MultiPV", 7)) {
				ptr += 14;
			    PVN = atoi(ptr);
				Stop = 1;
			} else if (!memcmp(ptr,"Ponder",6)) {
				ptr += 13;
				if (ptr[0] == 't') Ponder = 1;
				else Ponder = 0;
			} else if (!memcmp(ptr,"Clear",5)) {
				init_search(1);
				break;
			} else if (!memcmp(ptr,"PV",2)) {
				ptr += 14;
				if (ptr[0] == 't') PVHashing = 1;
				else PVHashing = 0;
			} else if (!memcmp(ptr, "Large", 5) && !Searching) {
				ptr += 25;
				if (ptr[0] == 't') {
					if (LargePages) return;
					LargePages = 1;
				} else {
					if (!LargePages) return;
					LargePages = 0;
				}
				ResetHash = 1;
				longjmp(ResetJump, 1);
			} else if (!memcmp(ptr, "Aspiration", 10)) {
				ptr += 24;
				if (ptr[0] == 't') Aspiration = 1;
				else Aspiration = 0;
			}
#ifdef CPU_TIMING
			else if (!memcmp(ptr, "CPUTiming", 9)) {
				ptr += 16;
				if (ptr[0] == 't') CpuTiming = 1;
				else CpuTiming = 0;
			} else if (!memcmp(ptr, "MaxDepth", 8)) UciMaxDepth = atoi(ptr + 15);
			else if (!memcmp(ptr, "MaxKNodes", 9)) UciMaxKNodes = atoi(ptr + 16);
			else if (!memcmp(ptr, "BaseTime", 8)) UciBaseTime = atoi(ptr + 15);
			else if (!memcmp(ptr, "IncTime", 7)) UciIncTime = atoi(ptr + 14);
#endif
        }
	} else if (!strcmp(mstring,"stop")) {
		Stop = 1;
		if (F(Searching)) send_best_move();
	} else if (!strcmp(mstring,"ponderhit")) {
		Infinite = 0;
		if (!RootList[1]) Stop = 1;
		if (F(CurrentSI->Bad) && F(CurrentSI->FailLow) && time_to_stop(BaseSI, LastTime, 0)) Stop = 1;
		if (F(Searching)) send_best_move();
	} else if (!strcmp(mstring, "quit")) {
		for (i = 1; i < PrN; i++) {
			TerminateProcess(ChildPr[i], 0);
			CloseHandle(ChildPr[i]);
		}
		exit(0);
	} else if (!memcmp(mstring, "epd", 3)) {
		ptr = mstring + 4;
		value = atoi(ptr);
		epd_test("op.epd", value);
	}
}

HANDLE CreateChildProcess(int child_id) {
	char name[1024];
	TCHAR szCmdline[1024];
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO siStartInfo;
	BOOL bSuccess = FALSE;

	ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));
	ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
	ZeroMemory(szCmdline, 1024 * sizeof(TCHAR));
	ZeroMemory(name, 1024);

	siStartInfo.cb = sizeof(STARTUPINFO);
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	GetModuleFileName(NULL, name, 1024);
	sprintf_s(szCmdline, " child %d %d", WinParId, child_id);

	bSuccess = CreateProcess(TEXT(name), TEXT(szCmdline), NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &siStartInfo, &piProcInfo);

	if (bSuccess) {
		CloseHandle(piProcInfo.hThread);
		return piProcInfo.hProcess;
	} else {
		fprintf(stdout, "Error %d\n", GetLastError());
		return NULL;
	}
}

int main(int argc, char *argv[]) {
	DWORD p;
	int i ;
	SYSTEM_INFO sysinfo;

	if (argc >= 2) if (!memcmp(argv[1], "child", 5)) {
		child = 1; parent = 0;
		WinParId = atoi(argv[2]);
		Id = atoi(argv[3]);
	}

	int CPUInfo[4] = { -1 };
	__cpuid(CPUInfo, 1);
	HardwarePopCnt = (CPUInfo[2] >> 23) & 1;

	if (parent) {
		if (((CPUInfo[3] >> 28) & 1) && GetProcAddress(GetModuleHandle(TEXT("kernel32")), "GetLogicalProcessorInformation") != NULL) {
			SYSTEM_LOGICAL_PROCESSOR_INFORMATION syslogprocinfo[1];
			p = sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
#ifdef WINDOWS_X64
			GetLogicalProcessorInformation(syslogprocinfo, &p);
			if (syslogprocinfo->ProcessorCore.Flags == 1) HT = 1;
#endif
		}
		WinParId = GetProcessId(GetCurrentProcess());
		HANDLE JOB = CreateJobObject(NULL, NULL);
		JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli = { 0 };
		jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
		SetInformationJobObject(JOB, JobObjectExtendedLimitInformation, &jeli, sizeof(jeli));
		AssignProcessToJobObject(JOB, GetCurrentProcess());
		if (MaxPrN > 1) {
			GetSystemInfo(&sysinfo);
			CPUs = sysinfo.dwNumberOfProcessors;
			PrN = Min(CPUs, MaxPrN);
			if (HT) PrN = Max(1, Min(PrN, CPUs / 2));
		}
	}

#ifdef CPU_TIMING
	SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
#endif

	init();

	StreamHandle = GetStdHandle(STD_INPUT_HANDLE);
	Console = GetConsoleMode(StreamHandle, &p);
	if (Console) {
		SetConsoleMode(StreamHandle, p & (~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT)));
		FlushConsoleInputBuffer(StreamHandle);
	}

	setbuf(stdout, NULL);
	setbuf(stdin, NULL);
	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stdin, NULL, _IONBF, 0);
	fflush(NULL);
	
	fprintf(stdout, "Chess engine %s%s%s\n", ENGINE, VERSION_, PLATFORM_);
	fprintf(stdout, "%s\n%s\n%s\n%s\n", AUTOR, MODS, CODES, CODES1);





reset_jump:

	if (parent) {
		if (setjmp(ResetJump)) {
			for (i = 1; i < PrN; i++) TerminateProcess(ChildPr[i], 0);
			for (i = 1; i < PrN; i++) {
				WaitForSingleObject(ChildPr[i], INFINITE);
				CloseHandle(ChildPr[i]);
			}
			Smpi->searching = Smpi->active_sp = Smpi->stop = 0;
			for (i = 0; i < MaxSplitPoints; i++) Smpi->Sp->active = Smpi->Sp->claimed = 0;
				
			Smpi->hash_size = hash_size;
			if (NewPrN) Smpi->PrN = PrN = NewPrN;
			goto reset_jump;
		}
		Smpi->hash_size = hash_size;
		Smpi->PrN = PrN;
	} else {
		hash_size = Smpi->hash_size;
		PrN = Smpi->PrN;
	}

	if (ResetHash) init_hash();
	init_search(0);

	if (child) while (true) check_state();
	if (parent) for (i = 1; i < PrN; i++) ChildPr[i] = CreateChildProcess(i);

	
	while (true) uci();
}