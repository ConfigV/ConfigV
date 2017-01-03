{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Benchmarks where

import           Settings
import           Types.Errors
import           Types.Rules
import           Types.IR

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           System.Directory
import           System.IO.Unsafe


learnTarget :: [ConfigFile Language]
learnTarget = case Settings.pROBRULES of
    Settings.Test -> genSet "testLearn/"
    Settings.NonProb -> genSet "benchmarkSet/correctMySQL/"
    Settings.Prob -> genSet "learningSet/MySQL/"
  where
    genSet s =
      map (\x -> (s++x,u $ T.readFile (s++x), MySQL))
      (u (listDirectory s))

-- verification file paths
vFilePaths :: [FilePath]
vFilePaths = if Settings.bENCHMARKS
    then benchmarkFiles
    else userFiles
  where
    userFiles = map ("user/"++) $ u $ listDirectory "user"
    benchmarkFiles = map getFileName $ concat benchmarks

benchmarks :: [ErrorReport]
benchmarks = case Settings.pROBRULES of
  Test -> testBenchSet
  NonProb -> group2 ++ group3 ++ group4 ++ group5
  Prob -> cavAE_benchmarks
  --Prob -> newSet --learnSetBench -- newSet--group2 -- ++ group4 ++ group5 -- ++ group6

u = unsafePerformIO
getFileName = fst . errLoc1

makeError (errLoc1,errLoc2,errIdent) =
  Error {errMsg=(show errIdent)++" SPEC - "++(show$snd errLoc1)++" AND "++(show$snd errLoc2),..}

newSet :: [ErrorReport]
newSet = map (map makeError) [
    [(("../OSDI/inject/correlation/error1.cnf","long_query_time[mysqld]"),("../OSDI/inject/correlation/error1.cnf","interactive_timeout[mysqld]"),INTREL)]
  , [(("../OSDI/inject/correlation/error2.cnf","max_allowed_packet[mysqldump]"),("../OSDI/inject/correlation/error2.cnf","key_buffer_size[myisamchk]"),INTREL)]
  , [(("../OSDI/inject/correlation/error3.cnf","thread_cache_size[mysqld]"),("../OSDI/inject/correlation/error3.cnf","table_cache[mysqld]"),INTREL)]

  , [(("../OSDI/inject/missing/error1.cnf","read_buffer[isamchk]"),("../OSDI/inject/missing/error1.cnf","write_buffer[isamchk]"),MISSING)]
  , [(("../OSDI/inject/missing/error2.cnf","read_buffer[myisamchk]"),("../OSDI/inject/missing/error2.cnf","write_buffer[myisamchk]"),MISSING)]

  , [(("../OSDI/inject/ordering/error1.cnf","datadir[mysqld]"),("../OSDI/inject/ordering/error1.cnf","basedir[mysqld]"),ORDERING)]
  , [(("../OSDI/inject/ordering/error2.cnf","tmpdir[mysqld]"),("../OSDI/inject/ordering/error2.cnf","basedir[mysqld]"),ORDERING)]
  , [(("../OSDI/inject/ordering/error3.cnf","basedir[mysqld]"),("../OSDI/inject/ordering/error3.cnf","pid-file[mysqld]"),ORDERING)]

  ]

newSet2 :: [ErrorReport]
newSet2 = map (map makeError) [
    [(("../OSDI/data/missing/case1","set_timeout[mysqld]"),("../OSDI/data/missing/case1","wait_timeout[mysqld]"),MISSING)]
  , [(("../OSDI/data/missing/case2","innodb_force_recovery[mysqld]"),("../OSDI/data/missing/case2","innodb_strict_mode[mysqld]"),MISSING)]

  , [(("../OSDI/data/typeError/case1","max_connections[mysqld]"),("../OSDI/data/typeError/case1","max_connections[mysqld]"),TYPE)]
  , [(("../OSDI/data/typeError/case2","innodb_data_file_path[mysqld]"),("../OSDI/data/typeError/case2","innodb_data_file_path[mysqld]"),TYPE)]

  , [(("../OSDI/data/correlation/case1","key_buffer_size[mysqld]"),("../OSDI/data/correlation/case1","sort_buffer_size[mysqld]"),INTREL)]
  , [(("../OSDI/data/correlation/case2","key_buffer_size[mysqld]"),("../OSDI/data/correlation/case2","join_buffer_size[mysqld]"),INTREL)]
  ]


cavAE_benchmarks =
    group2 ++ group3 ++ group4 ++ group5

group2 :: [ErrorReport]
group2 = map (map makeError) [
    [(("benchmarkSet/group2-entry-missing/error","port[client]"),("benchmarkSet/group2-entry-missing/error","socket[client]"),MISSING)]
  , [(("benchmarkSet/group2-entry-missing/error2","port[client]"),("benchmarkSet/group2-entry-missing/error2","socket[mysqld]"),MISSING)]
  , [(("benchmarkSet/group2-entry-missing/error3","port[client]"),("benchmarkSet/group2-entry-missing/error3","socket[mysqld]"),MISSING)]
  , [(("benchmarkSet/group2-entry-missing/error4","port[client]"),("benchmarkSet/group2-entry-missing/error4","socket[mysqld]"),MISSING)]
  , [(("benchmarkSet/group2-entry-missing/error5","socket[mysqld]"),("benchmarkSet/group2-entry-missing/error5","socket[mysqld]"),MISSING)]
  ]

group3 :: [ErrorReport]
group3 = map (map makeError) [
    [(("benchmarkSet/group3-path-type/error","general_log[mysqld]"),("benchmarkSet/group3-path-type/error","general_log[mysqld]"),TYPE)]
  , [(("benchmarkSet/group3-path-type/error2","query_cache_type[mysqld]"),("benchmarkSet/group3-path-type/error2","query_cache_type[mysqld]"),TYPE)]
  , [(("benchmarkSet/group3-path-type/error3","innodb_flush_log_at_trx_commit[wampmysqld]"),("benchmarkSet/group3-path-type/error3","innodb_flush_log_at_trx_commit[wampmysqld]"),TYPE)]
  , [(("benchmarkSet/group3-path-type/error4","character-set-server[mysqld]"),("benchmarkSet/group3-path-type/error4","character-set-server[mysqld]"),TYPE)]
  , [(("benchmarkSet/group3-path-type/error5","innodb_strict_mode[mysqld]"),("benchmarkSet/group3-path-type/error5","innodb_strict_mode[mysqld]"),TYPE)]
  ]

group4 :: [ErrorReport]
group4 = map (map makeError) [
    [(("benchmarkSet/group4-ordering/error_mysql","portport"),("benchmarkSet/group4-ordering/error_mysql","[client]"),ORDERING)]
  , [(("benchmarkSet/group4-ordering/error2","log-error[wampmysqld]"),("benchmarkSet/group4-ordering/error2","datadir[wampmysqld]"),ORDERING)]
  , [(("benchmarkSet/group4-ordering/error3","innodb_file_format_check[mysqld]"),("benchmarkSet/group4-ordering/error3","innodb_strict_mode[mysqld]"),ORDERING)]
  , [(("benchmarkSet/group4-ordering/error4","innodb_data_home_dir[wampmysqld]"),("benchmarkSet/group4-ordering/error4","innodb_flush_log_at_trx_commit[wampmysqld]"),ORDERING)]
  , [(("benchmarkSet/group4-ordering/error5","innodb_data_home_dir[mysqld]"),("benchmarkSet/group4-ordering/error5","innodb_file_format_check[mysqld]"),ORDERING)]
  ]

group5 :: [ErrorReport]
group5 = map (map makeError) [
    [(("benchmarkSet/group5-value-correlation/error","max_allowed_packet[wampmysqld"),("benchmarkSet/group5-value-correlation/error","key_buffer[wampmysqld]"),INTREL)]
  , [(("benchmarkSet/group5-value-correlation/error2","max_allowed_packet[wampmysqld]"),("benchmarkSet/group5-value-correlation/error2","key_buffer[wampmysqld]"),INTREL)]
  , [(("benchmarkSet/group5-value-correlation/error3","max_connections[mysqld]"),("benchmarkSet/group5-value-correlation/error3","connection_buffer[mysqld]"),INTREL)]
  , [(("benchmarkSet/group5-value-correlation/error4","max_allowed_packet[mysqldump]"),("benchmarkSet/group5-value-correlation/error4","key_buffer[mysqldump]"),INTREL)]
  , [(("benchmarkSet/group5-value-correlation/error5","key_buffer[isamchk]"),("benchmarkSet/group5-value-correlation/error5","sort_buffer_size[isamchk]"),INTREL)]
  ]

group6 :: [ErrorReport]
group6 = map (map makeError) [
    [(("benchmarkSet/realWorld/error1.cnf","max_allowed_packet[wampmysqld]"),("benchmarkSet/realWorld/error1.cnf","key_buffer[wampmysqld]"),INTREL)]
  , [(("benchmarkSet/realWorld/error2.cnf","max_allowed_packet[wampmysqld]"),("benchmarkSet/realWorld/error2.cnf","key_buffer[wampmysqld]"),INTREL)]
  , [(("benchmarkSet/realWorld/error3.cnf","max_connections[mysqld]"),("benchmarkSet/realWorld/error3.cnf","connection_buffer[mysqld]"),INTREL)]
  , [(("benchmarkSet/realWorld/error4.cnf","max_allowed_packet[mysqldump]"),("benchmarkSet/realWorld/error4.cnf","key_buffer[mysqldump]"),INTREL)]
  , [(("benchmarkSet/realWorld/error5.cnf","key_buffer[isamchk]"),("benchmarkSet/realWorld/error5.cnf","sort_buffer_size[isamchk]"),INTREL)]
  ]

testBenchSet :: [ErrorReport]
testBenchSet = map (map makeError) [
    [(("benchmarkSet/group5-value-correlation/test","a[mysqld]"),("benchmarkSet/group5-value-correlation/test","b[mysqld]"),INTREL)]
  ]
