

  data ConfigFile a = 
    if a == Common 
      then [(Variable,Value)]
      else [Text]
  
  translate :: ConfigFile a -> IRConfigFile
  translate ls = 
    map seperateVal ls

  seperateValues :: Text -> (Variables,Values)
  seperateValues t = 
    if (containsTypableValue t) |
      -> (getVariable t, getValue t)
       (contains "=" t) |
      -> breakOn "=" t


