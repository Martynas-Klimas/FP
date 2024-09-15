# fp-2024

Applications domain: Guitar shop.
# BNF
<guitar-shop> ::= <inventory>
<inventory> ::= <item-list>

<item-list> = <guitar> <amplifier> <accessory>

<add-item> = <guitar> | <amplifier> | <accessory>

<guitar> ::= <name> <price> <stock> <type>
<amplifier> ::= <name> <price> <stock> <type>
<accessory> ::= <name> <price> <stock> <type>

<name> ::= <string>
<stock> ::= <int> 
<price> ::= <int>
<type> ::= <string>

<string> ::= "" | <letter> <string>
<int> :: = <digit> | <digit> <int>
<letter> ::= "a" | "b" | "c" | ... | "z"
<digit> ::= "0" | "1" | "2" | ... | "9"


