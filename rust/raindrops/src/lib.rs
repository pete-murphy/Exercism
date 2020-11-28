pub fn raindrops(n: u32) -> String {
  let mut str = String::from("");
  if n % 3 == 0 {
    str = str + "Pling"
  }
  if n % 5 == 0 {
    str = str + "Plang"
  }
  if n % 7 == 0 {
    str = str + "Plong"
  }
  if str == "" {
    str = n.to_string()
  }
  str
}
