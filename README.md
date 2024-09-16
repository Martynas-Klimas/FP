# FP-2024 
# Application domain - guitar shop. 

Guitar shop has an inventory in which items such as guitars, amplifiers and other accesories are stored. It has a method by which it can add an item to its inventory.

## Main components:
1. item: Can be a guitar, amplifier or an accesory (pedal, pick, etc)

2. add_guitar | add_amplifier | add_accessory: each of these methods adds a certain item to the inventory.

3. guitar, amplifier, accesorry: each of the items has an id, name, price, stock, type and relates instrument which is the same type as itself.

4. related instrument: Can be a guitar, amplifier, accesory or be "None" if an item doesn't have anything related to it.

## Command examples:

- Adding a guitar item:
```
<add_guitar> ::= "AddGuitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"

AddGuitar(1,fender,1000,3,electric,none)
```

- Adding an amplifier item with a related amplifier:
```
<add_amplifier> ::= "AddAmplifier(" <id> "," <name> "," <price> "," <stock> "," <type> ","<related_amplifier> ")"

Amplifier(1, "Marshall JCM800", 900, 4, "Tube", Amplifier(2, "Fender Twin Reverb", 1300, 2, "Tube", None))
```

### BNF Grammar

```bnf
<guitar_shop> ::= <inventory>

<inventory> ::= <item_list>

<item_list> ::= <item> | <item> <item_list>
<item> ::= <guitar> <amplifier> <accessory>

<add_guitar> ::= "AddGuitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"

<add_amplifier> ::= "AddAmplifier(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_amplifier> ")"

<add_accessory> ::= "AddAccessory(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_accessory> ")"  

<guitar> ::= "Guitar(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"
<amplifier> ::= "Amplifier(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"
<accessory> ::= "Accessory(" <id> "," <name> "," <price> "," <stock> "," <type> "," <related_guitar> ")"

<related_guitar> ::= "none" | <guitar>
<related_amplifier> ::= "none" | <amplifier>
<related_accessory> ::= "none" | <accessory>

<id> ::= <int>
<name> ::= <string>
<stock> ::= <int>
<price> ::= <int>
<type> ::= <string>

<string> ::= <letter> | <letter> <string>
<int> ::= <digit> | <digit> <int>

<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" |
"g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" |
"p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

<digit> ::= "0" | "1" | "2" | "3" |
"4" | "5" | "6" | "7" | "8" | "9"
```