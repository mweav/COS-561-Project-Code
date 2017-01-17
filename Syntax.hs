{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
#-}

module Syntax where

import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Unbound.LocallyNameless

type TName = Name Ty

data Pol = Pos | Neg deriving Show

data Ty = Zero      Pol
        | One       Pol
        | Lolli     Pol Ty Ty
        | Choice    Pol Ty Ty
        | Var       Pol TName
        | Rec       Pol (Bind TName Ty)
        | CoRec     Pol (Bind TName Ty)
        deriving Show

dual_Pol :: Pol -> Pol
dual_Pol Pos = Neg
dual_Pol Neg = Pos

dual_var :: TName -> Ty -> FreshM Ty
dual_var n (Lolli p t t') = do
  s <- dual_var n t
  s' <- dual_var n t'
  return $ Lolli p s s'
dual_var n (Choice p t t') = do
  s <- dual_var n t
  s' <- dual_var n t'
  return $ Choice p s s'
dual_var n (Var p n') = case n == n' of
                          True -> return $ Var (dual_Pol p) n
                          False -> return $ Var p n'
dual_var n (Rec p b) = do
  (x, b') <- unbind b
  b'' <- dual_var n b'
  return $ Rec p (bind x b'')
dual_var n (CoRec p b) = do
  (x, b') <- unbind b
  b'' <- dual_var n b'
  return $ CoRec p (bind x b'')
dual_var n t = return t


dual' :: Ty -> FreshM Ty
dual' (Zero p)        = return $ Zero (dual_Pol p)
dual' (One p)         = return $ One (dual_Pol p)
dual' (Lolli p t t')  = do
  s <- dual' t
  s' <- dual' t'
  return $ Lolli (dual_Pol p) s s'
dual' (Choice p t t') = do
  s <- dual' t
  s' <- dual' t'
  return $ Choice (dual_Pol p) s s'
dual' (Var p n)       = return $ Var (dual_Pol p) n
dual' (Rec p b)       = do
  (x , b') <- unbind b
  b'' <- dual' b'
  b''' <- dual_var x b''
  return $ Rec (dual_Pol p) (bind x b''')
dual' (CoRec p b)     = do
  (x , b') <- unbind b
  b'' <- dual' b'
  b''' <- dual_var x b''
  return $ CoRec (dual_Pol p) (bind x b''')

dual t = runFreshM $ dual' t

$(derive [''Pol])
instance Alpha Pol 

$(derive [''Ty])

instance Alpha Ty

instance Subst Ty Pol where
  isvar _ = Nothing

instance Subst Ty Ty where
  isvar (Var p x) = Just (SubstName x)
  isvar _         = Nothing

{-
  subst n s (Zero p) = Zero p
  subst n s (One p) = One p
  subst n s (Lolli p t t') = Lolli p (subst n s t) (subst n s t')
  subst n s (Choice p t t') = Choice p (subst n s t) (subst n s t')
  subst n s (Var Pos n') = case n == n' of
                              True -> dual s
                              False -> (Var Pos n') 
  subst n s (Var Neg n') = case n == n' of
                              True -> s
                              False -> (Var Neg n')
  subst n s (Rec p b) = Rec p (subst n s b)
  subst n s (CoRec p b) = CoRec p (subst n s b)
-}

instance Eq Ty where
  (==) = aeq
                 
data Msg = End
         | Inl
         | Inr
         deriving Show

data DirMsg = Send Msg
            | Recv Msg
            deriving Show
