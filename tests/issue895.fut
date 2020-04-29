entry a =
  let scan' op ne as = scan op ne as
  in scan' (+) 0 []

entry b = a
