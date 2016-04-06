{-# LANGUAGE DataKinds #-}
module Triangle.FFI where

import qualified Foreign.Storable.Record as Store
import qualified Foreign.Storable.Traversable as TStore 
import Foreign.Storable.Record (element)
import Debug.Trace
import GHC.Natural
import Foreign.Storable (Storable (..), )
import GHC.Int


import Linear.V2
import Linear.V3
import qualified Data.Foldable as F
import qualified Foreign.Storable.Record as Store
import qualified Foreign.Storable.Traversable as TStore
import Foreign.Storable.Record (element)
import Foreign.Storable (Storable (..), )

import Foreign.Marshal hiding(peekArray)
import qualified Foreign.Marshal as Mar
import Foreign.Ptr

import Data.Vector (Vector)
import qualified Data.Vector as V





{#context prefix="triangle"  #}

peekArray i p 
  | trace ("peek" ++ show (i,p)) False = undefined
  | i == 0 || p == nullPtr = return []
  | otherwise = Mar.peekArray  i p 

data TriangulateIO 
  = TriangulateIO 
  { pointlist :: [V2 Double]
  , pointattributelist:: [Double]
  , pointmarkerlist :: [Int32]
  , numberofpoints :: Int 
  , numberofpointattributes :: Int 
  , trianglelist :: [V3 Int32]
  , triangleattributelist :: [Double]
  , trianglearealist :: [Double]
  , neighborlist :: [Int32]
  , numberoftriangles :: Int
  , numberofcorners :: Int
  , numberoftriangleattributes :: Int
  , segmentlist :: [V2 Int32]
  , segmentmarkerlist :: [Int32]
  , numberofsegments :: Int
  , holelist ::[V2 Double]
  , numberofholes :: Int
  , regionlist ::[Double]
  , numberofregions ::Int
  , edgelist :: [V2 Int32]
  , edgemarkerlist :: [Int32]
  , normlist :: [Double]
  , numberofedges :: Int
  }deriving Show

test = testTriangulate "pcBevz" itest
  
itest = (TriangulateIO ([V2 0 0,V2 0 3,V2 3 0,V2 3 3,V2 1 1,V2 1 2,V2 2 1,V2 2 2]) [] [] 8 0 [] [] [] [] 0 0 0  [V2 1 2,V2 5 7,V2 7 8,V2 8 6,V2 6 5] [] 5  [V2 1.5 1.5] 1 [] 0 [] [] [] 0 )

instance Storable TriangulateIO  where
   sizeOf _  = {# sizeof triangulateio #}
   alignment _ = {# alignof triangulateio #}

 
pokeArray' inp addr l  
  | F.null l = do
    pokeByteOff inp addr nullPtr
    return ()
  | otherwise = do
    newArray l >>= pokeByteOff inp addr
    return ()

pokeTrig inp = do
  inpp <- calloc
  pokeArray' inpp {# offsetof triangulateio.pointlist #} (pointlist inp)
  pokeArray' inpp {# offsetof triangulateio.pointattributelist #} (pointattributelist inp)
  pokeArray' inpp {# offsetof triangulateio.pointmarkerlist #} (pointmarkerlist inp)
  pokeArray' inpp {# offsetof triangulateio.segmentlist #} (segmentlist inp)

  pokeArray' inpp  {# offsetof triangulateio.segmentmarkerlist #} (segmentmarkerlist inp)
  pokeArray' inpp {# offsetof triangulateio.holelist #} (holelist inp)

  pokeArray' inpp {# offsetof triangulateio.edgelist#} (edgelist inp)
  pokeArray' inpp {# offsetof triangulateio.edgemarkerlist#} (edgemarkerlist inp)
  pokeArray' inpp {# offsetof triangulateio.normlist#} (normlist inp)

  pokeByteOff inpp {# offsetof triangulateio.numberofpoints #} (numberofpoints inp)
  pokeByteOff inpp {# offsetof triangulateio.numberofpointattributes #} (numberofpointattributes inp)
  pokeByteOff inpp {# offsetof triangulateio.numberoftriangles #} (numberoftriangles inp)
  pokeByteOff inpp {# offsetof triangulateio.numberofsegments #} (numberofsegments inp)
  pokeByteOff inpp {# offsetof triangulateio.numberofholes #} (numberofholes inp)
  pokeByteOff inpp {# offsetof triangulateio.numberofregions #} (numberofregions inp)
  pokeByteOff inpp {# offsetof triangulateio.numberofedges #} (numberofedges inp)
  return inpp

peekTrig outp = do
  numberofpoints <- fromIntegral <$>{# get triangulateio.numberofpoints #} outp 
  numberofpointattributes <- fromIntegral <$>{# get triangulateio.numberofpointattributes #} outp 

  numberoftriangles <- fromIntegral <$>{# get triangulateio.numberoftriangles #} outp 
  numberofcorners <-fromIntegral <$> {# get triangulateio.numberofcorners #} outp 
  numberoftriangleattributes <- fromIntegral <$>{# get triangulateio.numberoftriangleattributes #} outp 
  numberofsegments <- fromIntegral <$>{# get triangulateio.numberofsegments #} outp 
  numberofholes<- fromIntegral <$>{# get triangulateio.numberofholes #} outp 
  numberofregions <- fromIntegral <$>{# get triangulateio.numberofregions #} outp 

  numberofedges <- fromIntegral <$>{# get triangulateio.numberofedges #} outp 

  pl <- peekArray numberofpoints  .castPtr =<<  {# get triangulateio.pointlist #} outp 
  pal <- peekArray numberofpointattributes  .castPtr =<<  {# get triangulateio.pointattributelist #} outp 
  pml <- peekArray numberofpoints . castPtr =<<  {# get triangulateio.pointmarkerlist #} outp 
  tl <- peekArray numberoftriangles  . castPtr =<< {# get triangulateio.trianglelist #} outp 
  tal <- peekArray numberoftriangles  . castPtr =<< {# get triangulateio.triangleattributelist #} outp 
  taal <- peekArray numberoftriangles  . castPtr =<< {# get triangulateio.trianglearealist #} outp 
  nl <- peekArray numberofcorners  . castPtr =<< {# get triangulateio.neighborlist #} outp 
  sl <- peekArray numberofsegments . castPtr =<< {# get triangulateio.segmentlist #} outp 
  sml <- peekArray numberofsegments . castPtr =<< {# get triangulateio.segmentmarkerlist #} outp 
  hl <- peekArray numberofholes . castPtr =<< {# get triangulateio.holelist #} outp 
  rl <- peekArray numberofregions  . castPtr =<< {# get triangulateio.regionlist #} outp 
 
  el <- peekArray numberofedges  . castPtr =<< {# get triangulateio.edgelist #} outp 
  em  <- peekArray numberofedges . castPtr  =<< {# get triangulateio.edgemarkerlist #} outp 
  nol  <- peekArray numberofedges . castPtr  =<< {# get triangulateio.normlist #} outp 
  


  return (TriangulateIO pl pal pml numberofpoints numberofpointattributes tl tal taal nl numberoftriangles numberofcorners numberoftriangleattributes sl sml numberofsegments hl numberofholes rl numberofregions el em nol numberofedges)--(pl:: [V2 Double],el:: [V2 Int32] ,sl::[V2 Int32],tl::[V3 Int32])




testTriangulate  sw inp  = do
  inpp <- pokeTrig inp 
  outp <- pokeTrig inp
  vorp <- pokeTrig inp

  print "triangulating"
  triangulate sw inpp outp vorp
  print "triangulated"
  out <- peekTrig outp
  -- vor <- peekTrig vorp
  return (out,inp)

{#pointer * triangulateio as TriangulateIOPtr -> TriangulateIO #}

{# fun triangulate {`String',`TriangulateIOPtr',`TriangulateIOPtr',`TriangulateIOPtr'} -> `()' #}

