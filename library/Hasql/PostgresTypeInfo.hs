module Hasql.PostgresTypeInfo where

import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (bool)

-- | A Postgresql type info
data PTI = PTI {ptiOID :: OID, ptiArrayOID :: Maybe OID}

-- | A Word32 and a Pq representation of an OID
data OID = OID {oidWord32 :: Word32, oidPQ :: Pq.Oid, oidFormat :: Pq.Format}

mkOID :: Pq.Format -> Word32 -> OID
mkOID format x =
  OID x ((Pq.Oid . fromIntegral) x) format

mkPTI :: Pq.Format -> Word32 -> Maybe Word32 -> PTI
mkPTI format oid arrayOID =
  PTI (mkOID format oid) (fmap (mkOID format) arrayOID)

-- * Constants

abstime :: PTI
abstime = mkPTI Pq.Binary 702 (Just 1023)

aclitem :: PTI
aclitem = mkPTI Pq.Binary 1033 (Just 1034)

bit :: PTI
bit = mkPTI Pq.Binary 1560 (Just 1561)

bool :: PTI
bool = mkPTI Pq.Binary 16 (Just 1000)

box :: PTI
box = mkPTI Pq.Binary 603 (Just 1020)

bpchar :: PTI
bpchar = mkPTI Pq.Binary 1042 (Just 1014)

bytea :: PTI
bytea = mkPTI Pq.Binary 17 (Just 1001)

char :: PTI
char = mkPTI Pq.Binary 18 (Just 1002)

cid :: PTI
cid = mkPTI Pq.Binary 29 (Just 1012)

cidr :: PTI
cidr = mkPTI Pq.Binary 650 (Just 651)

circle :: PTI
circle = mkPTI Pq.Binary 718 (Just 719)

cstring :: PTI
cstring = mkPTI Pq.Binary 2275 (Just 1263)

date :: PTI
date = mkPTI Pq.Binary 1082 (Just 1182)

daterange :: PTI
daterange = mkPTI Pq.Binary 3912 (Just 3913)

datemultirange :: PTI
datemultirange = mkPTI Pq.Binary 4535 (Just 6155)

float4 :: PTI
float4 = mkPTI Pq.Binary 700 (Just 1021)

float8 :: PTI
float8 = mkPTI Pq.Binary 701 (Just 1022)

gtsvector :: PTI
gtsvector = mkPTI Pq.Binary 3642 (Just 3644)

inet :: PTI
inet = mkPTI Pq.Binary 869 (Just 1041)

int2 :: PTI
int2 = mkPTI Pq.Binary 21 (Just 1005)

int2vector :: PTI
int2vector = mkPTI Pq.Binary 22 (Just 1006)

int4 :: PTI
int4 = mkPTI Pq.Binary 23 (Just 1007)

int4range :: PTI
int4range = mkPTI Pq.Binary 3904 (Just 3905)

int4multirange :: PTI
int4multirange = mkPTI Pq.Binary 4451 (Just 6150)

int8 :: PTI
int8 = mkPTI Pq.Binary 20 (Just 1016)

int8range :: PTI
int8range = mkPTI Pq.Binary 3926 (Just 3927)

int8multirange :: PTI
int8multirange = mkPTI Pq.Binary 4536 (Just 6157)

interval :: PTI
interval = mkPTI Pq.Binary 1186 (Just 1187)

json :: PTI
json = mkPTI Pq.Binary 114 (Just 199)

jsonb :: PTI
jsonb = mkPTI Pq.Binary 3802 (Just 3807)

line :: PTI
line = mkPTI Pq.Binary 628 (Just 629)

lseg :: PTI
lseg = mkPTI Pq.Binary 601 (Just 1018)

macaddr :: PTI
macaddr = mkPTI Pq.Binary 829 (Just 1040)

money :: PTI
money = mkPTI Pq.Binary 790 (Just 791)

name :: PTI
name = mkPTI Pq.Binary 19 (Just 1003)

numeric :: PTI
numeric = mkPTI Pq.Binary 1700 (Just 1231)

numrange :: PTI
numrange = mkPTI Pq.Binary 3906 (Just 3907)

nummultirange :: PTI
nummultirange = mkPTI Pq.Binary 4532 (Just 6151)

oid :: PTI
oid = mkPTI Pq.Binary 26 (Just 1028)

oidvector :: PTI
oidvector = mkPTI Pq.Binary 30 (Just 1013)

path :: PTI
path = mkPTI Pq.Binary 602 (Just 1019)

point :: PTI
point = mkPTI Pq.Binary 600 (Just 1017)

polygon :: PTI
polygon = mkPTI Pq.Binary 604 (Just 1027)

record :: PTI
record = mkPTI Pq.Binary 2249 (Just 2287)

refcursor :: PTI
refcursor = mkPTI Pq.Binary 1790 (Just 2201)

regclass :: PTI
regclass = mkPTI Pq.Binary 2205 (Just 2210)

regconfig :: PTI
regconfig = mkPTI Pq.Binary 3734 (Just 3735)

regdictionary :: PTI
regdictionary = mkPTI Pq.Binary 3769 (Just 3770)

regoper :: PTI
regoper = mkPTI Pq.Binary 2203 (Just 2208)

regoperator :: PTI
regoperator = mkPTI Pq.Binary 2204 (Just 2209)

regproc :: PTI
regproc = mkPTI Pq.Binary 24 (Just 1008)

regprocedure :: PTI
regprocedure = mkPTI Pq.Binary 2202 (Just 2207)

regtype :: PTI
regtype = mkPTI Pq.Binary 2206 (Just 2211)

reltime :: PTI
reltime = mkPTI Pq.Binary 703 (Just 1024)

text :: PTI
text = mkPTI Pq.Binary 25 (Just 1009)

tid :: PTI
tid = mkPTI Pq.Binary 27 (Just 1010)

time :: PTI
time = mkPTI Pq.Binary 1083 (Just 1183)

timestamp :: PTI
timestamp = mkPTI Pq.Binary 1114 (Just 1115)

timestamptz :: PTI
timestamptz = mkPTI Pq.Binary 1184 (Just 1185)

timetz :: PTI
timetz = mkPTI Pq.Binary 1266 (Just 1270)

tinterval :: PTI
tinterval = mkPTI Pq.Binary 704 (Just 1025)

tsquery :: PTI
tsquery = mkPTI Pq.Binary 3615 (Just 3645)

tsrange :: PTI
tsrange = mkPTI Pq.Binary 3908 (Just 3909)

tsmultirange :: PTI
tsmultirange = mkPTI Pq.Binary 4533 (Just 6152)

tstzrange :: PTI
tstzrange = mkPTI Pq.Binary 3910 (Just 3911)

tstzmultirange :: PTI
tstzmultirange = mkPTI Pq.Binary 4534 (Just 6153)

tsvector :: PTI
tsvector = mkPTI Pq.Binary 3614 (Just 3643)

txid_snapshot :: PTI
txid_snapshot = mkPTI Pq.Binary 2970 (Just 2949)

textUnknown :: PTI
textUnknown = mkPTI Pq.Text 705 (Just 705)

binaryUnknown :: PTI
binaryUnknown = mkPTI Pq.Binary 705 (Just 705)

uuid :: PTI
uuid = mkPTI Pq.Binary 2950 (Just 2951)

varbit :: PTI
varbit = mkPTI Pq.Binary 1562 (Just 1563)

varchar :: PTI
varchar = mkPTI Pq.Binary 1043 (Just 1015)

void :: PTI
void = mkPTI Pq.Binary 2278 Nothing

xid :: PTI
xid = mkPTI Pq.Binary 28 (Just 1011)

xml :: PTI
xml = mkPTI Pq.Binary 142 (Just 143)
