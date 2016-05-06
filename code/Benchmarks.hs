{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Benchmarks where

import Types
import Settings

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.IO.Unsafe

import Debug.Trace

testLearnSet = genSet "testLearn/"
learningSet = genSet "dataset/correctMySQL/"
bigLearningSet = genSet  "dump/MySQL/"

genSet s =
  map (\x -> (u $ T.readFile (s++x), MySQL))
    (u (listDirectory s))

u = unsafePerformIO

-- | from the newest version of the package, which i cant get for some reason
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where
    isDir = not . u . doesDirectoryExist
    f filename = filename /= "." && filename /= ".." && isDir (path++filename)

makeError (errLoc1,errLoc2,errIdent) =
  Error {errMsg="Spec",..}
getFileName = fst . errLoc1

benchmarkFiles :: [FilePath]
benchmarkFiles = map getFileName $ concat benchmarks
benchmarks :: [ErrorReport]
benchmarks = case Settings.pROBRULES of
  Test -> testBenchSet
  NonProb -> group2 ++ group3 ++ group4 ++ group5
  Prob -> group2 -- ++ group4 ++ group5 -- ++ group6

group2 :: [ErrorReport]
group2 = map (map makeError) [
    [(("dataset/group2-entry-missing/error","port[client]"),("dataset/group2-entry-missing/error","socket[client]"),"MISSING")]
  , [(("dataset/group2-entry-missing/error2","port[client]"),("dataset/group2-entry-missing/error2","socket[mysqld]"),"MISSING")]
  , [(("dataset/group2-entry-missing/error3","port[client]"),("dataset/group2-entry-missing/error3","socket[mysqld]"),"MISSING")]
  , [(("dataset/group2-entry-missing/error4","port[client]"),("dataset/group2-entry-missing/error4","socket[mysqld]"),"MISSING")]
  , [(("dataset/group2-entry-missing/error5","socket[mysqld]"),("dataset/group2-entry-missing/error5","socket[mysqld]"),"MISSING")]
  ]

group3 :: [ErrorReport]
group3 = map (map makeError) [
    [(("dataset/group3-path-type/error","general_log[mysqld]"),("dataset/group3-path-type/error","general_log[mysqld]"),"TYPE")]
  , [(("dataset/group3-path-type/error2","query_cache_type[mysqld]"),("dataset/group3-path-type/error2","query_cache_type[mysqld]"),"TYPE")]
  , [(("dataset/group3-path-type/error3","innodb_flush_log_at_trx_commit[wampmysqld]"),("dataset/group3-path-type/error3","innodb_flush_log_at_trx_commit[wampmysqld]"),"TYPE")]
  , [(("dataset/group3-path-type/error4","character-set-server[mysqld]"),("dataset/group3-path-type/error4","character-set-server[mysqld]"),"TYPE")]
  , [(("dataset/group3-path-type/error5","innodb_strict_mode[mysqld]"),("dataset/group3-path-type/error5","innodb_strict_mode[mysqld]"),"TYPE")]
  ]

group4 :: [ErrorReport]
group4 = map (map makeError) [
    [(("dataset/group4-ordering/error_mysql","portport"),("dataset/group4-ordering/error_mysql","[client]"),"ORDERING")]
  , [(("dataset/group4-ordering/error2","datadir[wampmysqld]"),("dataset/group4-ordering/error2","log-error[wampmysqld]"),"ORDERING")]
  , [(("dataset/group4-ordering/error3","innodb_file_format_check[mysqld]"),("dataset/group4-ordering/error3","innodb_strict_mode[mysqld]"),"ORDERING")]
  , [(("dataset/group4-ordering/error4","innodb_data_home_dir[wampmysqld]"),("dataset/group4-ordering/error4","innodb_flush_log_at_trx_commit[wampmysqld]"),"ORDERING")]
  , [(("dataset/group4-ordering/error5","innodb_data_home_dir[mysqld]"),("dataset/group4-ordering/error5","innodb_file_format_check[mysqld]"),"ORDERING")]
  ]

group5 :: [ErrorReport]
group5 = map (map makeError) [
    [(("dataset/group5-value-correlation/error","max_allowed_packet[wampmysqld"),("dataset/group5-value-correlation/error","key_buffer[wampmysqld]"),"INTREL")]
  , [(("dataset/group5-value-correlation/error2","max_allowed_packet[wampmysqld]"),("dataset/group5-value-correlation/error2","key_buffer[wampmysqld]"),"INTREL")]
  , [(("dataset/group5-value-correlation/error3","max_connections[mysqld]"),("dataset/group5-value-correlation/error3","connection_buffer[mysqld]"),"INTREL")]
  , [(("dataset/group5-value-correlation/error4","max_allowed_packet[mysqldump]"),("dataset/group5-value-correlation/error4","key_buffer[mysqldump]"),"INTREL")]
  , [(("dataset/group5-value-correlation/error5","key_buffer[isamchk]"),("dataset/group5-value-correlation/error5","sort_buffer_size[isamchk]"),"INTREL")]
  ]

group6 :: [ErrorReport]
group6 = map (map makeError) [
    [(("dataset/realWorld/error1.cnf","max_allowed_packet[wampmysqld]"),("dataset/realWorld/error1.cnf","key_buffer[wampmysqld]"),"INTREL")]
  , [(("dataset/realWorld/error2.cnf","max_allowed_packet[wampmysqld]"),("dataset/realWorld/error2.cnf","key_buffer[wampmysqld]"),"INTREL")]
  , [(("dataset/realWorld/error3.cnf","max_connections[mysqld]"),("dataset/realWorld/error3.cnf","connection_buffer[mysqld]"),"INTREL")]
  , [(("dataset/realWorld/error4.cnf","max_allowed_packet[mysqldump]"),("dataset/realWorld/error4.cnf","key_buffer[mysqldump]"),"INTREL")]
  , [(("dataset/realWorld/error5.cnf","key_buffer[isamchk]"),("dataset/realWorld/error5.cnf","sort_buffer_size[isamchk]"),"INTREL")]
  ]

testBenchSet :: [ErrorReport]
testBenchSet = map (map makeError) [
    [(("dataset/group5-value-correlation/test","a[mysqld]"),("dataset/group5-value-correlation/test","b[mysqld]"),"INTREL")]
  ]
