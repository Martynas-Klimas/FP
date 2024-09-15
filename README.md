# FP-2024 
Application domain - guitar shop. Guitar shop has an inventory in which items such as guitars, amplifiers and other accesories are stored. It has a method by which it can add an item to its inventory.

Each of the items has a name, price, the amount of such items in store (stock) and type.

### BNF Grammar

```bnf
<guitar_shop> ::= <inventory>

<inventory> ::= <item_list>

<item_list> ::= <guitar> <amplifier> <accessory>

<add_guitar> ::= "AddGuitar(" <id> "," <name> "," <price> "," <stock> "," <type> ")"
<add_amplifier> ::= "AddAmplifier(" <id> "," <name> "," <price> "," <stock> "," <type> ")"
<add_accessory> ::= "AddAccessory(" <id> "," <name> "," <price> "," <stock> "," <type> ")"


<guitar> ::= "Guitar(" <id> "," <name> "," <price> "," <stock> "," <type> ")"
<amplifier> ::= "Amplifier(" <id> "," <name> "," <price> "," <stock> "," <type> ")"
<accessory> ::= "Accessory(" <id> "," <name> "," <price> "," <stock> "," <type> ")"

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