

    typeErrMsg x =
      Error {errLoc1 = (fname,fst x)
            ,errLoc2 = (fname,fst x)
            ,errIdent = TYPE
            ,errMsg = "TYPE ERROR: Expected a "++(show$snd x)++" for "++(show$fst x)++ ". Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }
    typeShow =
      showErr typeError typeErrMsg

