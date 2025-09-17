function divide_and_remainder(dividend, divisor)
  if divisor == 0 then
    tex.error("Error: Cannot divide by zero")
    return nil
  else
    local quotient = math.floor(dividend / divisor)
    local remainder = dividend % divisor
    return quotient, remainder
  end
end