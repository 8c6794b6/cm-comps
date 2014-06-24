{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|

Scratch module to play with building scnode and OSC messages.

-}
module Sound.Study.ForUserInterfaces.Scratch.NB where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Tree

import Sound.OSC
import Sound.SC3
import Sound.SC3.Tree hiding (grp, syn)

-- --------------------------------------------------------------------------
--
-- Monadic SCNode builder
--
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- From:
-- <http://stackoverflow.com/questions/20298842/how-do-i-convert-a-functional-dsl-into-a-monad-in-haskell>
--

data Ap a
    = ApRead (String -> Ap a)
    | ApWrite String (Ap a)
    | ApReturn a

instance Monad Ap where
    return = ApReturn
    m >>= k = case m of
        ApRead f     -> ApRead (\x -> f x >>= k)
        ApWrite x m' -> ApWrite x (m' >>= k)
        ApReturn x   -> k x

ioWriteLn x = ApWrite (x ++ "\n") (ApReturn ())
ioRead = ApRead ApReturn

runAp app = case app of
    ApRead k    -> runAp (k "Some input")
    ApReturn a  -> return a
    ApWrite s k -> putStr s >> runAp k

aptest :: Ap Int
aptest = do
    ioWriteLn "Hello World!"
    i <- ioRead
    ioWriteLn ("You wrote [" ++ i ++ "]")
    return 10

data Bld a
  = Gbld SCN ([SCN] -> Bld a)
  | Sbld SCN (SCN -> Bld a)
  | Nil a

instance Monad Bld where
    return = Nil
    m >>= k = case m of
        Gbld n f -> f [n] >>= k
        Sbld n f -> f n >>= k
        Nil a    -> k a

-- --------------------------------------------------------------------------
--
-- From: <http://echaozh.com/posts/monad-edsl.html>
--
-- --------------------------------------------------------------------------

newtype Stack a b = Stack {unStack :: State [a] b}
    deriving (Monad, Functor, Applicative, MonadState [a])

push :: a -> Stack a ()
push a = Stack $ modify (a:)

runStack :: Stack a b -> [a]
runStack s = execState (unStack s) []

-- >>> runStack stack01 == [3,2,1]
stack01 :: Stack Int ()
stack01 = do
    push 1
    push 2
    push 3

newtype Que a b = Que {unQue :: State ([a]->[a]) b}
    deriving (Monad, Functor, Applicative, MonadState ([a]->[a]))

que :: a -> Que a ()
que a = Que $ modify (. (a:))

runQue :: Que a b -> [a]
runQue q = execState (unQue q) id []

-- >>> runQue que01 == [1,2,3]
que01 :: Que Int ()
que01 = do
    que 1
    que 2
    que 3

-- Without using State monad.

newtype Que2 a b = Que2 {unQue2 :: ([a]->[a]) -> (b,[a]->[a])}

instance Functor (Que2 a) where
    fmap f (Que2 q) = Que2 $ \g -> let (x,g') = q g in (f x, g')
    {-# INLINE fmap #-}

instance Applicative (Que2 a) where
    pure x    = Que2 $ \f -> (x,f)
    {-# INLINE pure #-}
    q1 <*> q2 = q1 `ap` q2
    {-# INLINE (<*>) #-}

instance Monad (Que2 a) where
    return x = Que2 $ \f -> (x,f)
    {-# INLINE return #-}
    m >>= k  = Que2 $ \f -> let (x,f') = unQue2 m f in unQue2 (k x) f'
    {-# INLINE (>>=) #-}

que2 :: a -> Que2 a ()
que2 a = Que2 $ \f -> ((),f . (a:))

runQue2 :: Que2 a b -> [a]
runQue2 q = snd (unQue2 q id) []

-- >>> runQue2 que201 == [1,2,3]
que201 :: Que2 Int ()
que201 = do
    que2 1
    que2 2
    que2 3

-- Without using value, though using phantom type to make Monad instance.

type QueF a = [a] -> [a]
newtype Que3M a b = Que3M {unQue3M :: QueF a -> QueF a}

instance Functor (Que3M a) where
    fmap _ _ = Que3M id

instance Applicative (Que3M a) where
    pure _ = Que3M id
    (<*>)  = ap

instance Monad (Que3M a) where
    return _ = Que3M id
    m >>= k  = Que3M $ \f -> let f' = unQue3M m f in unQue3M (k undefined) f'

type Que3 a = Que3M a ()

que3 :: a -> Que3 a
que3 x = Que3M $ \f -> f . (x:)

runQue3 :: Que3 a -> [a]
runQue3 q = unQue3M q id []

-- >>> runQue3 que301 == [1,2,3]
que301 :: Que3 Int
que301 = do
    que3 1
    que3 2
    que3 3


-- --------------------------------------------------------------------------
--
-- Monadic functions to build SCNode
--
-- --------------------------------------------------------------------------

type Builder a b = State (BuilderState a) b

data BuilderState a = BuilderState
    { bsFunc           :: [a] -> [a]
    , bsCurrentGroupId :: Int -- lazy
    , bsCurrentOffset  :: Int -- lazy
    }

grp :: Int -> Builder SCNode () -> Builder SCNode ()
grp n childs = modify $ \st ->
    let st0     = builderState n
        childs' = runBuilder childs st0 []
    in  st { bsFunc           = bsFunc st . (Group n childs' :)
           , bsCurrentGroupId = n
           , bsCurrentOffset  = 0
           }
{-# INLINE grp #-}

syn :: String -> Builder SynthParam () -> Builder SCNode ()
syn name paramBuilder =
    modify $ \st ->
    let myId      = (bsCurrentGroupId st * 100) + bsCurrentOffset st
        newOffset = bsCurrentOffset st + 1
        params    = runBuilder paramBuilder (builderState undefined) []
    in  st { bsFunc = bsFunc st . (Synth myId name params:)
           , bsCurrentOffset = newOffset
           }
{-# INLINE syn #-}

(==>) :: Parameter p => String -> p -> Builder SynthParam ()
name ==> param = modify $ \st ->
    let (_,pval) = parameterize name param
    in st { bsFunc = bsFunc st . (pval:) }

nil :: Builder a ()
nil = return ()
{-# INLINE nil #-}

builderState :: Int -> BuilderState a
builderState n = BuilderState
    { bsFunc           = id
    , bsCurrentGroupId = n
    , bsCurrentOffset  = 0
    }

runBuilder :: Builder a b -> BuilderState a -> [a] -> [a]
runBuilder m st0 acc =
    let st1 = execState m st0
    in  bsFunc st1 acc

build :: Builder a b -> a
build builder =
    case runBuilder builder (builderState 0) [] of
        n:_ -> n
        _   -> error "build: empty list"

class Parameter p where
    parameterize :: String -> p -> ([Message],SynthParam)

instance Parameter Double where
    parameterize name value = ([], name := value)

dbl :: Double -> Double
dbl = id

instance Audible (Builder SCNode ()) where
    play = patchNode . build

instance Show (Builder SCNode ()) where
    showsPrec _ n = \str -> str ++ drawSCNode (build n)


n200 =
    grp 0 $ do
        grp 1 nil
        grp 2 nil

n201 =
    grp 0 $
        grp 1 $ do
            syn "foo" nil
            syn "bar" nil

n202 = do
    grp 0 $ do
        grp 1 $ do
            grp 10 $ do
               syn "foo" $ do
                   "freq" ==> dbl 440
                   "amp"  ==> dbl 0.3
               syn "bar" nil
        grp 2 $ do
            syn "foo" nil
            syn "bar" nil
            syn "buzz" nil

n203 = do
    grp 0 $ do
        grp 1 $ do
            grp 10 $ do
                forM_ [100,101..116] $ \n -> grp n nil
            grp 11 nil
        grp 2 nil
