lastButOne list = if length list <= 2
                      then head list
                  else 
                      head (drop (length list - 2) list)
