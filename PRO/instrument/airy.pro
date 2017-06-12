function airy, r, phi

  a = 1/(1 + (4 * r * sin(phi/2)^2) / (1-r)^2)
  return, a

end

