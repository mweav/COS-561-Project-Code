{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
#-}

module Monitor where

import Syntax
import Data.Bool
import Data.Tuple
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Unbound.LocallyNameless


{- we have 3 things in state:
   + Current type we are checking against
   + Polarity of Lolli Dividing current type from jump type
   + Type we jump to if we use all of current type
-}
type St = (Maybe Ty , Maybe Pol , Maybe Ty)

transition' :: DirMsg -> St -> FreshM (Maybe St)
transition' m (Just (Zero _), q, t) = return $ Just (t , Nothing , Nothing)
transition' (Recv End) (Just (One Pos), q, t) = return $ Just (t, Nothing, Nothing)
transition' (Send End) (Just (One Neg), q, t) = return $ Just (t, Nothing, Nothing)
transition' (Recv m) (Just (Lolli Pos t u), Just q, Just t') = transition' (Recv m) (Just t, Just Pos, Just (Lolli q u t')) 
transition' (Recv m) (Just (Lolli Pos t u), Nothing, Nothing) = transition' (Recv m) (Just t, Just Pos, Just u) 
transition' (Send m) (Just (Lolli Neg t u), Just q, Just t') = transition' (Send m) (Just t, Just Neg, Just (Lolli q u t'))
transition' (Send m) (Just (Lolli Neg t u), Nothing, Nothing) = transition' (Send m) (Just t, Just Neg, Just u)
transition' (Recv Inl) (Just (Choice Pos t u), q, t') = return $ Just (Just t, q, t')
transition' (Send Inl) (Just (Choice Neg t u), q, t') = return $ Just (Just t, q, t')
transition' (Recv Inr) (Just (Choice Pos t u), q, t') = return $ Just (Just u, q, t')
transition' (Send Inr) (Just (Choice Neg t u), q, t') = return $ Just (Just u, q, t')
transition' m (Just (Rec p bnd), q, t) = do
  (x, b) <- unbind bnd
  transition' m (Just (subst x (Rec p (bind x b)) b), q, t)
transition' m (Just (CoRec p bnd), q, t) = do
  (x, b) <- unbind bnd
  transition' m (Just (subst x (CoRec p (bind x b)) b), q, t) 
transition' _ _ = return Nothing

transition :: DirMsg -> St -> Maybe St
transition m s = runFreshM $ transition' m s

-- Returns true if the protocol has been followed up thus far
monitor :: State (Maybe St) Bool
monitor = do
  s <- get
  case s of
    Just _  -> return True
    Nothing -> return False

-- Initializes the monitor
initMonitor :: Ty -> State (Maybe St) Bool
initMonitor t = do
  put (Just (Just t , Nothing , Nothing))
  monitor

-- Transitions the monitor with a new message
transMonitor :: DirMsg -> State (Maybe St) Bool
transMonitor m = do
  s <- get
  case s of
    Just s' -> do
      put (transition m s')
      monitor
    Nothing -> monitor

-- Returns true if the protocol has already succeeded and terminated
isSuccess :: State (Maybe St) Bool
isSuccess = do
  s <- get
  case s of
    Just (Nothing, Nothing, Nothing) -> return True
    _ -> return False

eval :: State (Maybe St) a -> a
eval s = fst (runState s Nothing)

evalSt :: State (Maybe St) a -> Maybe St
evalSt s = snd (runState s Nothing)

nat :: Ty
nat = Rec Neg (bind (s2n "x") (Choice Neg (One Neg) (Var Neg (s2n "x"))))

witness_three :: State (Maybe St) Bool
witness_three = do
  initMonitor nat
  transMonitor (Send Inr)
  transMonitor (Send Inr)
  transMonitor (Send Inr)
  transMonitor (Send Inl)
  transMonitor (Send End)

offer_nats :: Ty
offer_nats = CoRec Pos (bind (s2n "x") (Choice Pos (Lolli Neg nat (Var Pos (s2n "x"))) (One Neg)))

witness_send_0_1_2 :: State (Maybe St) Bool
witness_send_0_1_2 = do
  initMonitor offer_nats
  transMonitor (Recv Inl)
  transMonitor (Send Inl)
  transMonitor (Send End)
  transMonitor (Recv Inl)
  transMonitor (Send Inr)
  transMonitor (Send Inl)
  transMonitor (Send End)
  transMonitor (Recv Inl)
  transMonitor (Send Inr)
  transMonitor (Send Inr)
  transMonitor (Send Inl)
  transMonitor (Send End)
  transMonitor (Recv Inr)
  transMonitor (Send End)

success_send_0_1_2 = witness_send_0_1_2 >> isSuccess

{- To try it out, evaluate programs such as the following:

  eval (initMonitor (Choice Neg (One Pos) (One Neg)) >> transMonitor (Send Inr) >> transMonitor (Send End))
  eval (initMonitor (Rec Neg (bind (s2n "x") (Choice Neg (One Neg) (Var Neg (s2n "x"))))) >> transMonitor (Send Inr) >> transMonitor (Send Inr) >> transMonitor (Send Inr))
  eval (initMonitor (Rec Neg (bind (s2n "x") (Choice Neg (One Neg) (Var Neg (s2n "x"))))) >> transMonitor (Send Inr) >> transMonitor (Send Inl) >> transMonitor (Send End))

-}

add1 :: Ty
add2 :: Ty
mul1 :: Ty
mul2 :: Ty
exp1 :: Ty
exp2 :: Ty
sendNat :: Ty
recvNat :: Ty

sendNat = nat
recvNat = CoRec Neg (bind (s2n "x") (Choice Pos (One Pos) (Var Pos (s2n "x"))))


add1 = Lolli Neg recvNat sendNat
mul1 = Lolli Neg recvNat sendNat
exp1 = Lolli Neg recvNat sendNat
add2 = Lolli Neg (Lolli Neg recvNat recvNat) sendNat
mul2 = Lolli Neg (Lolli Neg recvNat recvNat) sendNat
exp2 = Lolli Neg (Lolli Neg recvNat recvNat) sendNat

menu :: Ty
menu = Choice Neg (Choice Neg (Choice Neg (add1) (add2)) (Choice Neg (mul1) (mul2))) (Choice Neg (exp1) (exp2))

operator :: Ty
operator = Lolli Neg recvNat menu

calculator :: Ty
calculator = CoRec Pos (bind (s2n "x") (Choice Pos (Lolli Neg operator (Var Pos (s2n "x"))) (One Neg)))


 






