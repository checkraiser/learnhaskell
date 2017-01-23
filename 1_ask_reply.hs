-- program ask your name
-- you answer
-- program: reply with "Hello" using the name you entered
main = do
  putStrLn "What's your name ? "
  x <- getLine
  let answer = "Hello " ++ x
  putStrLn answer
