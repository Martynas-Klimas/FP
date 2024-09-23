module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- main keywords
    "guitar_shop",
    "inventory",
    "item_list",
    "item",
    "add_guitar",
    "add_amplifier",
    "add_accessory",

    --items 
    "guitar",
    "amplifier",
    "accessory",
    "related_guitar",
    "related_amplifier",
    "related_accessory",

    --properties
    "name",
    "price",
    "stock",
    "type"
    ]
