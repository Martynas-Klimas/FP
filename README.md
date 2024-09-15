# FP-2024 
Application domain - guitar shop. Guitar shop has an inventory in which items such as guitars, amplifiers and other accesories are stored. It has a method by which it can add an item to its inventory.

Each of the items has a name, price, the amount of such items in store (stock) and type.

### BNF Grammar

```bnf
<guitar-shop> ::= <inventory>

<inventory> ::= <item-list>

<item-list> = <guitar> <amplifier> <accessory>

<add-item> ::= <guitar> | <amplifier> | <accessory>

<guitar> ::= <name> <price> <stock> <type>
<amplifier> ::= <name> <price> <stock> <type>
<accessory> ::= <name> <price> <stock> <type>

<name> ::= <string>
<stock> ::= <int>
<price> ::= <int>
<type> ::= <string>

<string> ::= "" | <letter> <string>
<int> ::= <digit> | <digit> <int>

<letter> ::= "a" | "b" | "c" | ... | "z"
<digit> ::= "0" | "1" | "2" | ... | "9"


