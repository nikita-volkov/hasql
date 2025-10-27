module Hasql.Codecs.TypeInfo where

import Hasql.Platform.Prelude hiding (bool)

-- | A Postgresql type info
data TypeInfo = TypeInfo {toBaseOid :: Word32, toArrayOid :: Word32}

abstime :: TypeInfo
abstime = TypeInfo 702 1023

aclitem :: TypeInfo
aclitem = TypeInfo 1033 1034

bit :: TypeInfo
bit = TypeInfo 1560 1561

bool :: TypeInfo
bool = TypeInfo 16 1000

box :: TypeInfo
box = TypeInfo 603 1020

bpchar :: TypeInfo
bpchar = TypeInfo 1042 1014

bytea :: TypeInfo
bytea = TypeInfo 17 1001

char :: TypeInfo
char = TypeInfo 18 1002

cid :: TypeInfo
cid = TypeInfo 29 1012

cidr :: TypeInfo
cidr = TypeInfo 650 651

circle :: TypeInfo
circle = TypeInfo 718 719

cstring :: TypeInfo
cstring = TypeInfo 2275 1263

date :: TypeInfo
date = TypeInfo 1082 1182

daterange :: TypeInfo
daterange = TypeInfo 3912 3913

datemultirange :: TypeInfo
datemultirange = TypeInfo 4535 6155

float4 :: TypeInfo
float4 = TypeInfo 700 1021

float8 :: TypeInfo
float8 = TypeInfo 701 1022

gtsvector :: TypeInfo
gtsvector = TypeInfo 3642 3644

inet :: TypeInfo
inet = TypeInfo 869 1041

int2 :: TypeInfo
int2 = TypeInfo 21 1005

int2vector :: TypeInfo
int2vector = TypeInfo 22 1006

int4 :: TypeInfo
int4 = TypeInfo 23 1007

int4range :: TypeInfo
int4range = TypeInfo 3904 3905

int4multirange :: TypeInfo
int4multirange = TypeInfo 4451 6150

int8 :: TypeInfo
int8 = TypeInfo 20 1016

int8range :: TypeInfo
int8range = TypeInfo 3926 3927

int8multirange :: TypeInfo
int8multirange = TypeInfo 4536 6157

interval :: TypeInfo
interval = TypeInfo 1186 1187

json :: TypeInfo
json = TypeInfo 114 199

jsonb :: TypeInfo
jsonb = TypeInfo 3802 3807

line :: TypeInfo
line = TypeInfo 628 629

lseg :: TypeInfo
lseg = TypeInfo 601 1018

macaddr :: TypeInfo
macaddr = TypeInfo 829 1040

money :: TypeInfo
money = TypeInfo 790 791

name :: TypeInfo
name = TypeInfo 19 1003

numeric :: TypeInfo
numeric = TypeInfo 1700 1231

numrange :: TypeInfo
numrange = TypeInfo 3906 3907

nummultirange :: TypeInfo
nummultirange = TypeInfo 4532 6151

oid :: TypeInfo
oid = TypeInfo 26 1028

oidvector :: TypeInfo
oidvector = TypeInfo 30 1013

path :: TypeInfo
path = TypeInfo 602 1019

point :: TypeInfo
point = TypeInfo 600 1017

polygon :: TypeInfo
polygon = TypeInfo 604 1027

record :: TypeInfo
record = TypeInfo 2249 2287

refcursor :: TypeInfo
refcursor = TypeInfo 1790 2201

regclass :: TypeInfo
regclass = TypeInfo 2205 2210

regconfig :: TypeInfo
regconfig = TypeInfo 3734 3735

regdictionary :: TypeInfo
regdictionary = TypeInfo 3769 3770

regoper :: TypeInfo
regoper = TypeInfo 2203 2208

regoperator :: TypeInfo
regoperator = TypeInfo 2204 2209

regproc :: TypeInfo
regproc = TypeInfo 24 1008

regprocedure :: TypeInfo
regprocedure = TypeInfo 2202 2207

regtype :: TypeInfo
regtype = TypeInfo 2206 2211

reltime :: TypeInfo
reltime = TypeInfo 703 1024

text :: TypeInfo
text = TypeInfo 25 1009

tid :: TypeInfo
tid = TypeInfo 27 1010

time :: TypeInfo
time = TypeInfo 1083 1183

timestamp :: TypeInfo
timestamp = TypeInfo 1114 1115

timestamptz :: TypeInfo
timestamptz = TypeInfo 1184 1185

timetz :: TypeInfo
timetz = TypeInfo 1266 1270

tinterval :: TypeInfo
tinterval = TypeInfo 704 1025

tsquery :: TypeInfo
tsquery = TypeInfo 3615 3645

tsrange :: TypeInfo
tsrange = TypeInfo 3908 3909

tsmultirange :: TypeInfo
tsmultirange = TypeInfo 4533 6152

tstzrange :: TypeInfo
tstzrange = TypeInfo 3910 3911

tstzmultirange :: TypeInfo
tstzmultirange = TypeInfo 4534 6153

tsvector :: TypeInfo
tsvector = TypeInfo 3614 3643

txid_snapshot :: TypeInfo
txid_snapshot = TypeInfo 2970 2949

unknown :: TypeInfo
unknown = TypeInfo 705 705

uuid :: TypeInfo
uuid = TypeInfo 2950 2951

varbit :: TypeInfo
varbit = TypeInfo 1562 1563

varchar :: TypeInfo
varchar = TypeInfo 1043 1015

xid :: TypeInfo
xid = TypeInfo 28 1011

xml :: TypeInfo
xml = TypeInfo 142 143
