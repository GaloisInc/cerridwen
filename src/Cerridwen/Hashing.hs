{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hashing where
import qualified Data.Parameterized.Map as PM
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.NatRepr as PR
import qualified Data.Macaw.Types as MT
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.X86 as MX
import qualified Data.Macaw.X86.ArchTypes as MXA
import qualified Flexdis86 as FD

-- Instances of Hashable

-- from generic
instance (PC.Hashable FD.Segment)

instance (PC.Hashable MXA.AVXOp1) where
  hashWithSalt s = \case
    MXA.VShiftL i -> s `PC.hashWithSalt` (0::Int) `PC.hashWithSalt` i
    MXA.VShiftR i -> s `PC.hashWithSalt` (1::Int) `PC.hashWithSalt` i
    MXA.VShufD  i -> s `PC.hashWithSalt` (2::Int) `PC.hashWithSalt` i

instance (PC.Hashable MXA.AVXOp2) where
  hashWithSalt s = \case
    MXA.VPAnd        -> s `PC.hashWithSalt` (0::Int)
    MXA.VPOr         -> s `PC.hashWithSalt` (1::Int)
    MXA.VPXor        -> s `PC.hashWithSalt` (2::Int)
    MXA.VPAlignR i   -> s `PC.hashWithSalt` (3::Int) `PC.hashWithSalt` i
    MXA.VPShufB      -> s `PC.hashWithSalt` (4::Int)
    MXA.VAESEnc      -> s `PC.hashWithSalt` (5::Int)
    MXA.VAESEncLast  -> s `PC.hashWithSalt` (6::Int)
    MXA.VPCLMULQDQ i -> s `PC.hashWithSalt` (7::Int) `PC.hashWithSalt` i
    MXA.VPUnpackLQDQ -> s `PC.hashWithSalt` (8::Int)
    MXA.VPUnpackHQDQ -> s `PC.hashWithSalt` (9::Int)

instance (PC.Hashable MXA.AVXPointWiseOp2) where
  hashWithSalt s = \case
    MXA.PtAdd -> s `PC.hashWithSalt` (0::Int)
    MXA.PtSub -> s `PC.hashWithSalt` (1::Int)
    MXA.PtCmpGt -> s `PC.hashWithSalt` (2::Int)

instance (PC.Hashable (MXA.SIMDByteCount w)) where
  hashWithSalt s = \case
    MXA.SIMDByteCount_MMX -> s `PC.hashWithSalt` (0::Int)
    MXA.SIMDByteCount_XMM -> s `PC.hashWithSalt` (1::Int)
    MXA.SIMDByteCount_YMM -> s `PC.hashWithSalt` (2::Int)

instance (PC.Hashable (MXA.SSE_Op)) where
  hashWithSalt s = \case
    MXA.SSE_Add -> s `PC.hashWithSalt` (0::Int)
    MXA.SSE_Sub -> s `PC.hashWithSalt` (1::Int)
    MXA.SSE_Mul -> s `PC.hashWithSalt` (2::Int)
    MXA.SSE_Div -> s `PC.hashWithSalt` (3::Int)
    MXA.SSE_Min -> s `PC.hashWithSalt` (4::Int)
    MXA.SSE_Max -> s `PC.hashWithSalt` (5::Int)

instance (PC.Hashable (MXA.SSE_Cmp)) where
  hashWithSalt s = \case
    MXA.EQ_OQ -> s `PC.hashWithSalt` (0::Int)
    MXA.LT_OS -> s `PC.hashWithSalt` (1::Int)
    MXA.LE_OS -> s `PC.hashWithSalt` (2::Int)
    MXA.UNORD_Q -> s `PC.hashWithSalt` (3::Int)
    MXA.NEQ_UQ -> s `PC.hashWithSalt` (4::Int)
    MXA.NLT_US -> s `PC.hashWithSalt` (5::Int)
    MXA.NLE_US -> s `PC.hashWithSalt` (6::Int)
    MXA.ORD_Q -> s `PC.hashWithSalt` (7::Int)
 
instance (PC.Hashable (MXA.SSE_FloatType tp)) where
  hashWithSalt s = \case
    MXA.SSE_Single -> s `PC.hashWithSalt` (0::Int)
    MXA.SSE_Double -> s `PC.hashWithSalt` (1::Int)

instance (PC.HashableF MXA.SSE_FloatType) where
  hashWithSaltF = PC.hashWithSalt

instance (PC.Hashable (MXA.RepValSize w)) where
  hashWithSalt s x = s `PC.hashWithSalt` (MXA.repValSizeByteCount x)

instance PC.HashableF f => PC.Hashable (MX.X86Stmt f) where
  hashWithSalt s = \case
    MX.SetSegmentSelector sg v -> (s `PC.hashWithSalt` (0::Int) `PC.hashWithSalt` sg) `PC.hashWithSaltF` v
    MX.StoreX87Control addr -> s `PC.hashWithSalt` (1::Int) `PC.hashWithSaltF` addr
    MX.RepMovs bc dest src cnt dir -> (s `PC.hashWithSalt` (2::Int)  `PC.hashWithSalt` bc) `PC.hashWithSaltF` dest `PC.hashWithSaltF` src `PC.hashWithSaltF` cnt `PC.hashWithSaltF` dir
    MX.RepStos bc dest val cnt dir -> (s `PC.hashWithSalt` (3::Int) `PC.hashWithSalt` bc) `PC.hashWithSaltF` dest `PC.hashWithSaltF` val `PC.hashWithSaltF` cnt `PC.hashWithSaltF` dir
    MX.EMMS ->  s `PC.hashWithSalt` (4::Int) 
  
instance PC.HashableF f => PC.Hashable (MX.X86PrimFn f tp) where
  hashWithSalt s = \case
      MX.EvenParity x -> s `PC.hashWithSalt` (0::Int) `PC.hashWithSaltF` x
      MX.ReadFSBase -> s `PC.hashWithSalt` (1::Int) 
      MX.ReadGSBase -> s `PC.hashWithSalt` (2::Int) 
      MX.GetSegmentSelector sl -> s `PC.hashWithSalt` (3::Int) `PC.hashWithSalt` sl
      MX.CPUID v    -> s `PC.hashWithSalt` (4::Int) `PC.hashWithSaltF` v
      MX.CMPXCHG8B a ax bx cx dx  -> s `PC.hashWithSalt` (5::Int) `PC.hashWithSaltF` a `PC.hashWithSaltF` ax `PC.hashWithSaltF` bx `PC.hashWithSaltF` cx `PC.hashWithSaltF` dx
      MX.RDTSC      -> s `PC.hashWithSalt` (6::Int) 
      MX.XGetBV v   -> s `PC.hashWithSalt` (7::Int) `PC.hashWithSaltF` v
      MX.PShufb w x y -> (s `PC.hashWithSalt` (8::Int) `PC.hashWithSalt` w) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.MemCmp sz cnt src dest rev -> (s `PC.hashWithSalt` (9::Int) `PC.hashWithSalt` sz) `PC.hashWithSaltF` cnt `PC.hashWithSaltF` src `PC.hashWithSaltF` dest `PC.hashWithSaltF` rev
      MX.RepnzScas sz val buf cnt -> (s `PC.hashWithSalt` (10::Int) `PC.hashWithSalt` sz) `PC.hashWithSaltF` val `PC.hashWithSaltF` buf `PC.hashWithSaltF` cnt
      MX.MMXExtend v -> s `PC.hashWithSalt` (11::Int) `PC.hashWithSaltF` v
      MX.X86IDivRem w num1 num2 d -> (s `PC.hashWithSalt` (12::Int) `PC.hashWithSalt` w) `PC.hashWithSaltF` num1 `PC.hashWithSaltF` num2 `PC.hashWithSaltF` d
      MX.X86DivRem  w num1 num2 d -> (s `PC.hashWithSalt` (13::Int) `PC.hashWithSalt` w) `PC.hashWithSaltF` num1 `PC.hashWithSaltF` num2 `PC.hashWithSaltF` d
      MX.SSE_UnaryOp op tp x y -> (s `PC.hashWithSalt` (14::Int) `PC.hashWithSalt` op) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.SSE_VectorOp op n tp x y -> (s `PC.hashWithSalt` (15::Int) `PC.hashWithSalt` op `PC.hashWithSalt` n) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.SSE_Sqrt ftp x ->  s `PC.hashWithSalt` (16::Int) `PC.hashWithSaltF` ftp `PC.hashWithSaltF` x
      MX.SSE_CMPSX c tp x y ->  (s `PC.hashWithSalt` (17::Int) `PC.hashWithSalt` c) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.SSE_UCOMIS tp x y ->  s `PC.hashWithSalt` (18::Int) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.SSE_CVTSS2SD x ->  s `PC.hashWithSalt` (19::Int) `PC.hashWithSaltF` x
      MX.SSE_CVTSD2SS x ->  s `PC.hashWithSalt` (20::Int) `PC.hashWithSaltF` x
      MX.SSE_CVTSI2SX tp w  x -> s `PC.hashWithSalt` (21::Int) `PC.hashWithSaltF` tp `PC.hashWithSaltF` w `PC.hashWithSaltF` x
      MX.SSE_CVTTSX2SI w tp x -> (s `PC.hashWithSalt` (22::Int) `PC.hashWithSalt` w) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x
      MX.X87_Extend tp x ->  s `PC.hashWithSalt` (23::Int) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x
      MX.X87_FAdd x y ->  s `PC.hashWithSalt` (24::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.X87_FSub x y ->  s `PC.hashWithSalt` (25::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.X87_FMul x y ->  s `PC.hashWithSalt` (26::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.X87_FST tp x ->  s `PC.hashWithSalt` (27::Int) `PC.hashWithSaltF` tp `PC.hashWithSaltF` x
      MX.VOp1 w o x   ->  (s `PC.hashWithSalt` (28::Int) `PC.hashWithSalt` w `PC.hashWithSalt` o) `PC.hashWithSaltF` x
      MX.VOp2 w o x y ->  (s `PC.hashWithSalt` (29::Int) `PC.hashWithSalt` w `PC.hashWithSalt` o) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.PointwiseShiftL e n sh x y ->  (s `PC.hashWithSalt` (30::Int) `PC.hashWithSalt` e `PC.hashWithSalt` n `PC.hashWithSalt` sh) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.PointwiseLogicalShiftR e n sh x y ->  (s `PC.hashWithSalt` (31::Int) `PC.hashWithSalt` e `PC.hashWithSalt` n `PC.hashWithSalt` sh) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.Pointwise2 n w o x y ->  (s `PC.hashWithSalt` (32::Int) `PC.hashWithSalt` n `PC.hashWithSalt` w `PC.hashWithSalt` o) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.VExtractF128 x i ->  s `PC.hashWithSalt` (33::Int) `PC.hashWithSaltF` x `PC.hashWithSalt` i
      MX.VInsert n w v e i ->  (s `PC.hashWithSalt` (34::Int) `PC.hashWithSalt` n `PC.hashWithSalt` w) `PC.hashWithSaltF` v `PC.hashWithSaltF` e `PC.hashWithSalt` i
      MX.CLMul x y ->  s `PC.hashWithSalt` (35::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.AESNI_AESEnc x y ->  s `PC.hashWithSalt` (36::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.AESNI_AESEncLast x y ->  s `PC.hashWithSalt` (37::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.AESNI_AESDec x y ->  s `PC.hashWithSalt` (38::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.AESNI_AESDecLast x y ->  s `PC.hashWithSalt` (39::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y
      MX.AESNI_AESKeyGenAssist x i ->  s `PC.hashWithSalt` (40::Int) `PC.hashWithSaltF` x `PC.hashWithSalt` i
      MX.AESNI_AESIMC x ->  s `PC.hashWithSalt` (41::Int) `PC.hashWithSaltF` x 
      MX.SHA_sigma0 x ->  s `PC.hashWithSalt` (42::Int) `PC.hashWithSaltF` x 
      MX.SHA_sigma1 x ->  s `PC.hashWithSalt` (43::Int) `PC.hashWithSaltF` x 
      MX.SHA_Sigma0 x ->  s `PC.hashWithSalt` (44::Int) `PC.hashWithSaltF` x 
      MX.SHA_Sigma1 x ->  s `PC.hashWithSalt` (45::Int) `PC.hashWithSaltF` x 
      MX.SHA_Ch x y z ->  s `PC.hashWithSalt` (46::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y `PC.hashWithSaltF` z
      MX.SHA_Maj x y z ->  s `PC.hashWithSalt` (47::Int) `PC.hashWithSaltF` x `PC.hashWithSaltF` y `PC.hashWithSaltF` z 

instance PC.HashableF f => PC.HashableF (MX.X86PrimFn f) where
  hashWithSaltF = PC.hashWithSalt

instance (PC.HashableF a, PC.HashableF b) => PC.Hashable (PM.Pair a b) where
  hashWithSalt s (PM.Pair a b) = s `PC.hashWithSaltF` a `PC.hashWithSaltF` b

instance PC.Hashable (MC.AssignId id tp) where
  hashWithSalt s (MC.AssignId n) = PC.hashWithSalt s n

instance PC.HashableF (MC.AssignId id) where
  hashWithSaltF = PC.hashWithSalt

instance PC.Hashable (MT.TypeRepr tp) where
  hashWithSalt s = \case
    MT.BoolTypeRepr -> s `PC.hashWithSalt` (0::Int)
    MT.BVTypeRepr nrep -> s `PC.hashWithSalt` (1::Int) `PC.hashWithSalt` (PR.intValue nrep)
    MT.FloatTypeRepr frep -> s `PC.hashWithSalt` (2::Int) `PC.hashWithSalt` frep
    MT.TupleTypeRepr treps -> s `PC.hashWithSalt` (MC.foldlFC PC.hashWithSalt (3::Int) treps)
    MT.VecTypeRepr nrep trep -> s `PC.hashWithSalt` (4::Int) `PC.hashWithSalt` (PR.intValue nrep) `PC.hashWithSalt` trep
      

-- instance (PC.HashableF f, PC.HashableF (MC.ArchFn arch), PC.HashableF (MC.ArchStmt arch))
instance (PC.HashableF f , PC.HashableF (MC.ArchFn arch f)) => PC.Hashable (MC.AssignRhs arch f tp) where
  hashWithSalt s = \case
    MC.EvalApp app -> s `PC.hashWithSalt` (0::Int) `PC.hashWithSalt` app
    MC.SetUndefined rep -> s `PC.hashWithSalt` (1::Int) `PC.hashWithSalt` rep
    MC.ReadMem bv rep -> (s `PC.hashWithSalt` (2::Int)) `PC.hashWithSaltF` bv `PC.hashWithSalt` rep
    MC.CondReadMem rep b bv tp -> (s `PC.hashWithSalt` (3::Int) `PC.hashWithSalt` rep) `PC.hashWithSaltF` b `PC.hashWithSaltF` bv `PC.hashWithSaltF` tp
    MC.EvalArchFn arch rep -> ((s `PC.hashWithSalt` (4::Int)) `PC.hashWithSaltF` arch) `PC.hashWithSalt` rep
      

instance (PC.HashableF f, PC.HashableF (MC.ArchFn arch f)) => PC.HashableF (MC.AssignRhs arch f) where
  hashWithSaltF = PC.hashWithSalt



instance (PC.HashableF (MC.Value arch ids), PC.HashableF (MC.ArchFn arch (MC.Value arch ids))) => PC.Hashable (MC.Assignment arch ids tp) where
  hashWithSalt s (MC.Assignment aid rhs) = (s `PC.hashWithSalt` aid) `PC.hashWithSalt` rhs

instance (PC.HashableF (MC.Value arch ids), PC.HashableF (MC.ArchFn arch (MC.Value arch ids))) => PC.HashableF (MC.Assignment arch ids) where
  hashWithSaltF = PC.hashWithSalt

instance (PC.HashableF (MC.Value arch ids), PC.HashableF (MC.ArchReg arch), PC.HashableF (MC.ArchFn arch (MC.Value arch ids))) => PC.Hashable (MC.Value arch ids tp) where
  hashWithSalt s = \case 
    MC.CValue cval -> s `PC.hashWithSalt` (0::Int) `PC.hashWithSalt` cval
    MC.AssignedValue a -> (s `PC.hashWithSalt` (1::Int)) `PC.hashWithSalt` a
    MC.Initial reg -> s `PC.hashWithSalt` (2::Int) `PC.hashWithSaltF` reg

instance (PC.HashableF (MC.Value arch ids), PC.HashableF (MC.ArchReg arch), PC.HashableF (MC.ArchFn arch (MC.Value arch ids))) => PC.HashableF (MC.Value arch ids) where
  hashWithSaltF = PC.hashWithSalt

instance (PC.HashableF (MC.Value arch ids), PC.HashableF (MC.ArchReg arch), PC.HashableF (MC.ArchFn arch (MC.Value arch ids)), PC.Hashable (MC.ArchStmt arch (MC.Value arch ids))) => PC.Hashable (MC.Stmt arch ids) where
  hashWithSalt s = \case
    MC.AssignStmt a -> (s `PC.hashWithSalt` (0::Int)) `PC.hashWithSalt` a
    MC.WriteMem a mrep rhs -> ((s `PC.hashWithSalt` (1::Int)) `PC.hashWithSaltF` a `PC.hashWithSaltF` mrep) `PC.hashWithSalt` rhs
    MC.CondWriteMem c a mrep rhs -> ((s `PC.hashWithSalt` (2::Int) `PC.hashWithSalt` c) `PC.hashWithSaltF` a `PC.hashWithSaltF` mrep) `PC.hashWithSalt` rhs
    MC.InstructionStart off t -> s `PC.hashWithSalt` (3::Int) `PC.hashWithSalt` off `PC.hashWithSalt` t
    MC.Comment t -> s `PC.hashWithSalt` (4::Int) `PC.hashWithSalt` t
    MC.ExecArchStmt stm -> s `PC.hashWithSalt` (5::Int) `PC.hashWithSalt` stm
    MC.ArchState a _ -> s `PC.hashWithSalt` (6::Int) `PC.hashWithSalt` a 

