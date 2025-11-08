\ file dev-prop.fth
\ purpose: Data structure and manipulation for all USB devices
\

hex
\ device-prop ( DP): data structure for USB driver.
\ It keeps track of all devicesa and their properties



begin-structure /dp-entry
    field:  >dp-class
    field:  >dp-subclass
    field:  >dp-protocol

end-structure

