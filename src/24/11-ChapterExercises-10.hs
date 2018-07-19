{-# LANGUAGE OverloadedStrings #-}

module ChapterExercises10 where

-- Write a parser for the DOT language
import           Data.Text
import           Text.Trifecta

data GraphType
  = UndirectedGraph
  | DirectedGraph
  deriving (Eq, Show)

type AttrName = Text

type AttrValue = Text
