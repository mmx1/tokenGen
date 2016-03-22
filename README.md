# tokenGen - a token-based passphrase generator

Usage: tokenGen-exe --path TARGET [-e|--entropy ARG] [-f|--fill] [-d|--detailed]

  Builds a vocabulary from the content of the specified path and creates a 2^n
  bit entropy password (default 75)

Available options:

  -h,--help                Show this help text
  
  --path TARGET            Path TARGET to search for words
  
  -e,--entropy ARG         Number of bits of entropy desired
  
  -f,--fill                Fill up to 2^32 with the most common English words
  
  -d,--detailed            Perform detailed analysis
