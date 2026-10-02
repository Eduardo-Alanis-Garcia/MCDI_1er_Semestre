t = (10, 20, 30)

t[1] * t[3] - t[2]



struct Point
  x::Float32
  y::Float32
end


"""
  Calcula la norma de un vector representado
  como una tupla
"""
function norm(u::Tuple)
  s = 0f0
  for i in eachindex(u)
    s += u[i]^2
  end
  sqrt(s)
end

"""
  Calcula la norma de un vector de 2 dimensiones
  representado como una estructura
"""
function norm(u::Point)
  sqrt(u.x^2 + u.y^2)
end

(norm((1, 1, 1, 1)), norm(Point(1, 1)))



function mydot(u, x)
  s = 0f0
  for i in eachindex(u, x)
    s += u[i] * x[i]
  end
  s
end

function getmaxdot(u::Vector, X::Matrix)
  maxpos = 1
  # en la siguiente linea, @view nos permite controlar que
  # no se copien los arreglos, y en su lugar, se usen referencias
  maxdot = mydot(u, @view X[:, 1])
  # obtiene el número de columnas e itera a partir del 2do índice
  mfilas, ncols = size(X)
  for i in 2:ncols
    d = mydot(u, @view X[:, i]) 
    if d > maxdot
      maxpos = i
      maxdot = d
    end
  end

  (maxpos, maxdot)
end

getmaxdot(rand(Float32, 4), rand(Float32, 4, 1000))