# FP-2024 
# Application domain - guitar shop. 

Guitar shop has an inventory in which items such as guitars, amplifiers and other accesories are stored. It has a method by which it can add an item to its inventory.

### Lib3 Batch queries
Now it is possible to provide many queries at the same time, running them as a batch.
After loading the program, you can run it using `stack run fp2024-three`, then write the following commands:

```
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| AddGuitar(1,Fender,20,200,Electric,none);
| AddGuitar(1,Gibson,20,200,Acoustic,none);
| AddAmplifier(2,Marshall,10,500,Tube,Amplifier(5,Marshall,10,300,other,none));
| END
| 
Guitar added successfully
Guitar added successfully
Amplifier added successfully
```

Save is used to save data to a file (state.txt):
```
>>> save
State saved successfully
```

In state.txt the saved state looks like this: 
```
BEGIN
AddAmplifier(2,Marshall,10,500,Tube,Amplifier(5,Marshall,10,300,other,none));
AddGuitar(1,Gibson,20,200,Acoustic,none);
AddGuitar(1,Fender,20,200,Electric,none);
END
```
We can close the seesion, then run `stack run fp2024-three` again to load data from state.txt.
Here data is being loaded successfully and ViewInventory command confirms this.
```
>>> load
Amplifier added successfully
Guitar added successfully
Guitar added successfully
Guitar added successfully
>>> ViewInventory
Inventory Guitar: GuitarData {guitarId = 1, guitarName = "Name", guitarStock = 200, guitarPrice = 20, guitarType = "Type", relatedGuitar = Nothing}
Guitar: GuitarData {guitarId = 1, guitarName = "Fender", guitarStock = 200, guitarPrice = 20, guitarType = "Electric", relatedGuitar = Nothing}
Guitar: GuitarData {guitarId = 1, guitarName = "Gibson", guitarStock = 200, guitarPrice = 20, guitarType = "Acoustic", relatedGuitar = Nothing}
Amplifier: AmplifierData {amplifierId = 2, amplifierName = "Marshall", amplifierStock = 500, amplifierPrice = 10, amplifierType = "Tube", relatedAmplifier = Just (AmplifierData {amplifierId = 5, amplifierName = "Marshall", amplifierStock = 300, amplifierPrice = 10, amplifierType = "other", relatedAmplifier = Nothing})}
```

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