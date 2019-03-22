module MadLib where

import Data.Monoid

type Verb        = String
type Adverb      = String
type Adjective   = String
type Noun        = String
type Exclamation = String

madlibbin ::
    Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbin e adv n adj =
    e   <> "! he said " <>
    adv <> " as he jumped into his car " <>
    n   <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' ::
    Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbinBetter' e adv n adj =
    let story =
            [
             e
            ,"! he said "
            ,adv
            ," as he jumped into his car "
            ,n
            ," and drove off with his "
            ,adj
            ," wife."
            ]

    in mconcat story
