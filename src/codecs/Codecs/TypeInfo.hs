module Codecs.TypeInfo where

import Platform.Prelude hiding (bool)

-- | A Postgresql type info
data TypeInfo = TypeInfo {toBaseOid :: Word32, toArrayOid :: Word32}

bool :: TypeInfo
bool = TypeInfo 16 1000

bytea :: TypeInfo
bytea = TypeInfo 17 1001

char :: TypeInfo
char = TypeInfo 18 1002

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

inet :: TypeInfo
inet = TypeInfo 869 1041

int2 :: TypeInfo
int2 = TypeInfo 21 1005

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

macaddr :: TypeInfo
macaddr = TypeInfo 829 1040

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

record :: TypeInfo
record = TypeInfo 2249 2287

text :: TypeInfo
text = TypeInfo 25 1009

time :: TypeInfo
time = TypeInfo 1083 1183

timestamp :: TypeInfo
timestamp = TypeInfo 1114 1115

timestamptz :: TypeInfo
timestamptz = TypeInfo 1184 1185

timetz :: TypeInfo
timetz = TypeInfo 1266 1270

tsrange :: TypeInfo
tsrange = TypeInfo 3908 3909

tsmultirange :: TypeInfo
tsmultirange = TypeInfo 4533 6152

tstzrange :: TypeInfo
tstzrange = TypeInfo 3910 3911

tstzmultirange :: TypeInfo
tstzmultirange = TypeInfo 4534 6153

unknown :: TypeInfo
unknown = TypeInfo 705 705

uuid :: TypeInfo
uuid = TypeInfo 2950 2951
