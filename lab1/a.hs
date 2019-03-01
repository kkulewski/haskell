-- ZAD 1

-- ZAD 2
gcd2 x y =
  if x /= y
    then if x < y
      then gcd x (y-x)
      else gcd (x-y) y
  else
    x


-- ZAD 3
smaller x y = x < y
greater x y = y < x

equal x y =
  if smaller x y
    then False
    else if greater x y
      then False
      else True

smaller_equal x y =
  if greater x y
    then False
    else True

greater_equal x y =
  if smaller x y
    then False
    else True

not_equal x y =
  if equal x y
    then False
    else True


-- ZAD 4
even1 x = (x `mod` 2) == 0
odd1 x = not (even1 x)

-- ZAD 5
plus x y = x + y
times x y = x * y

same_values p1 p2 x y =
  if p1 x y == p2 x y
    then True
    else False


-- ZAD 6
delta a b c = (b * b) - (4 * a * c)

kwad a b c = (
    ((-b) + sqrt (delta a b c)) / (2 * a),
    ((-b) - sqrt (delta a b c)) / (2 * a)
  )
