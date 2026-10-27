# Ejercicio 6: minimización del costo del petróleo
# Métodos: Newton con búsqueda lineal de Armijo y quasi-Newton BFGS.
# El script usa únicamente funciones de R base.

options(scipen = 999)

# Parámetros del modelo
a = 0.20
c_c = 12.5
c_i = 0.50
c_x = 0.90
i_rate = 0.10
n_ports = 2
p_land = 7000
conversion_bbl_per_kL = 6.29

# Exponentes
r_exp = -0.3017
s_exp = 0.4925
u_exp = 0.7952
v_exp = 0.861
w_exp = -0.1899
m_exp = 0.671

# Coeficientes de la función compacta 
c0 = c_c + c_i + c_x
A = 2.09e4 / 360
B = 1.064e6 / (52.47 * 360)
C = 4.242e4 * a / (52.47 * 360)
D = 1.813 * i_rate * p_land / (52.47 * 360)
E = 4.25e3 * a / (52.47 * 360)
F = 5.042e3 / 360
G = 0.1049 / 360

# Escalas de las variables, en las unidades usadas por el enunciado.
scale_t = 1e5 # kL
scale_q = 1e5 # bbl/día

# Función de costo c(t,q), en dólares por kL.
oil_cost = function(t, q) {
  if (length(t) != 1L || length(q) != 1L ||
      !is.finite(t) || !is.finite(q) || t <= 0 || q <= 0) {
    return(Inf)
  }

  S = n_ports * t + 1.2 * q
  c0 +
    A * t^r_exp +
    B * t^s_exp / q +
    C * t^u_exp / q +
    D * S^v_exp / q +
    E * S / q +
    F * q^w_exp +
    G * q^m_exp
}

# Gradiente analítico respecto de (t,q).
oil_gradient = function(t, q) {
  S = n_ports * t + 1.2 * q

  c_t = A * r_exp * t^(r_exp - 1) +
    B * s_exp * t^(s_exp - 1) / q +
    C * u_exp * t^(u_exp - 1) / q +
    D * v_exp * n_ports * S^(v_exp - 1) / q +
    E * n_ports / q

  c_q = -B * t^s_exp / q^2 -
    C * t^u_exp / q^2 +
    D * (-S^v_exp / q^2 + 1.2 * v_exp * S^(v_exp - 1) / q) +
    E * (-S / q^2 + 1.2 / q) +
    F * w_exp * q^(w_exp - 1) +
    G * m_exp * q^(m_exp - 1)

  c(c_t = c_t, c_q = c_q)
}

# Hessiana analítica respecto de (t,q).
oil_hessian = function(t, q) {
  S = n_ports * t + 1.2 * q

  c_tt = A * r_exp * (r_exp - 1) * t^(r_exp - 2) +
    B * s_exp * (s_exp - 1) * t^(s_exp - 2) / q +
    C * u_exp * (u_exp - 1) * t^(u_exp - 2) / q +
    D * v_exp * (v_exp - 1) * n_ports^2 * S^(v_exp - 2) / q

  c_tq = -B * s_exp * t^(s_exp - 1) / q^2 -
    C * u_exp * t^(u_exp - 1) / q^2 +
    D * v_exp * n_ports *
      (-S^(v_exp - 1) / q^2 +
         1.2 * (v_exp - 1) * S^(v_exp - 2) / q) -
    E * n_ports / q^2

  c_qq = 2 * B * t^s_exp / q^3 +
    2 * C * t^u_exp / q^3 +
    D * (2 * S^v_exp / q^3 -
           2.4 * v_exp * S^(v_exp - 1) / q^2 +
           1.44 * v_exp * (v_exp - 1) * S^(v_exp - 2) / q) +
    E * (2 * S / q^3 - 2.4 / q^2) +
    F * w_exp * (w_exp - 1) * q^(w_exp - 2) +
    G * m_exp * (m_exp - 1) * q^(m_exp - 2)

  matrix(c(c_tt, c_tq, c_tq, c_qq), nrow = 2L, byrow = TRUE,
         dimnames = list(c("t", "q"), c("t", "q")))
}

# Transformaciones entre variables físicas (t,q) y escaladas (tau,kappa).
# La regla de la cadena se aplica tanto al gradiente como a la Hessiana.
to_physical = function(z) {
  c(t = scale_t * z[1], q = scale_q * z[2])
}

scaled_cost = function(z) {
  physical = to_physical(z)
  oil_cost(physical["t"], physical["q"])
}

scaled_gradient = function(z) {
  physical = to_physical(z)
  c(scale_t, scale_q) *
    oil_gradient(physical["t"], physical["q"])
}

scaled_hessian = function(z) {
  physical = to_physical(z)
  scale_matrix = diag(c(scale_t, scale_q))
  scale_matrix %*%
    oil_hessian(physical["t"], physical["q"]) %*%
    scale_matrix
}

# Búsqueda lineal de Armijo; descarta puntos fuera del dominio t,q > 0.
armijo_search = function(z, direction, gradient, value,
                          c1 = 1e-4, contraction = 0.5,
                          min_step = 2^-50) {
  slope = sum(gradient * direction)
  if (!is.finite(slope) || slope >= 0) {
    return(NULL)
  }

  step = 1
  while (step >= min_step) {
    candidate = z + step * direction
    if (all(is.finite(candidate)) && all(candidate > 0)) {
      candidate_value = scaled_cost(candidate)
      if (is.finite(candidate_value) &&
          candidate_value <= value + c1 * step * slope) {
        return(list(z = candidate, value = candidate_value, step = step))
      }
    }
    step = contraction * step
  }

  NULL
}

newton_method = function(z0 = c(tau = 0.5, kappa = 1),
                          tolerance = 1e-9, max_iterations = 100L) {
  z = as.numeric(z0)

  for (iteration in 0:max_iterations) {
    value = scaled_cost(z)
    gradient = scaled_gradient(z)
    gradient_norm = sqrt(sum(gradient^2))

    if (gradient_norm < tolerance) {
      return(list(z = z, value = value, iterations = iteration,
                  gradient_norm = gradient_norm, converged = TRUE))
    }
    if (iteration == max_iterations) {
      break
    }

    hessian = scaled_hessian(z)
    newton_step = tryCatch(
      as.vector(solve(hessian, gradient)),
      error = function(e) rep(NA_real_, 2L)
    )
    direction = -newton_step

    # Si la Hessiana no produce descenso, usar -gradiente como respaldo.
    if (any(!is.finite(direction)) || sum(gradient * direction) >= 0) {
      direction = -gradient
    }

    search = armijo_search(z, direction, gradient, value)
    if (is.null(search)) {
      stop("Newton: la búsqueda de Armijo no encontró un paso aceptable.")
    }
    z = search$z
  }

  list(z = z, value = scaled_cost(z), iterations = max_iterations,
       gradient_norm = sqrt(sum(scaled_gradient(z)^2)), converged = FALSE)
}

bfgs_method = function(z0 = c(tau = 0.5, kappa = 1),
                        tolerance = 1e-9, max_iterations = 200L) {
  z = as.numeric(z0)
  inverse_hessian = diag(2L)

  for (iteration in 0:max_iterations) {
    value = scaled_cost(z)
    gradient = scaled_gradient(z)
    gradient_norm = sqrt(sum(gradient^2))

    if (gradient_norm < tolerance) {
      return(list(z = z, value = value, iterations = iteration,
                  gradient_norm = gradient_norm, converged = TRUE))
    }
    if (iteration == max_iterations) {
      break
    }

    direction = -as.vector(inverse_hessian %*% gradient)
    if (any(!is.finite(direction)) || sum(gradient * direction) >= 0) {
      inverse_hessian = diag(2L)
      direction = -gradient
    }

    search = armijo_search(z, direction, gradient, value)
    if (is.null(search)) {
      stop("BFGS: la búsqueda de Armijo no encontró un paso aceptable.")
    }

    z_next = search$z
    gradient_next = scaled_gradient(z_next)
    s = z_next - z
    y = gradient_next - gradient
    ys = sum(y * s)

    # Actualización BFGS de la Hessiana inversa aproximada.
    if (is.finite(ys) && ys > 1e-12 * sqrt(sum(s^2) * sum(y^2))) {
      rho = 1 / ys
      identity = diag(2L)
      left = identity - rho * tcrossprod(s, y)
      right = identity - rho * tcrossprod(y, s)
      inverse_hessian = left %*% inverse_hessian %*% right +
        rho * tcrossprod(s, s)
    } else {
      inverse_hessian = diag(2L)
    }

    z = z_next
  }

  list(z = z, value = scaled_cost(z), iterations = max_iterations,
       gradient_norm = sqrt(sum(scaled_gradient(z)^2)), converged = FALSE)
}

# Diferencias finitas centrales para contrastar el gradiente y la Hessiana
# analíticos en las coordenadas escaladas.
finite_difference_gradient = function(z, step = 1e-4) {
  result = numeric(2L)
  for (j in 1:2) {
    e = numeric(2L)
    e[j] = step
    result[j] = (scaled_cost(z + e) - scaled_cost(z - e)) / (2 * step)
  }
  result
}

finite_difference_hessian = function(z, step = 1e-3) {
  result = matrix(0, nrow = 2L, ncol = 2L)
  f0 = scaled_cost(z)

  for (j in 1:2) {
    e = numeric(2L)
    e[j] = step
    result[j, j] = (scaled_cost(z + e) - 2 * f0 +
                       scaled_cost(z - e)) / step^2
  }

  e1 = c(step, 0)
  e2 = c(0, step)
  mixed = (scaled_cost(z + e1 + e2) -
              scaled_cost(z + e1 - e2) -
              scaled_cost(z - e1 + e2) +
              scaled_cost(z - e1 - e2)) / (4 * step^2)
  result[1, 2] = mixed
  result[2, 1] = mixed
  result
}

report_result = function(method_name, result) {
  if (!result$converged) {
    warning(method_name, " no alcanzó la tolerancia solicitada.")
  }

  physical = to_physical(result$z)
  eigenvalues = eigen(scaled_hessian(result$z),
                       symmetric = TRUE, only.values = TRUE)$values

  cat("\n", method_name, "\n", sep = "")
  cat(sprintf("Iteraciones: %d\n", result$iterations))
  cat(sprintf("tau = %.10f, kappa = %.10f\n", result$z[1], result$z[2]))
  cat(sprintf("t* = %.4f kL (%.4f bbl)\n",
              physical["t"], physical["t"] * conversion_bbl_per_kL))
  cat(sprintf("q* = %.4f bbl/día (%.4f kL/día)\n",
              physical["q"], physical["q"] / conversion_bbl_per_kL))
  cat(sprintf("Costo = %.10f dólares/kL\n", result$value))
  cat(sprintf("Norma del gradiente escalado = %.3e\n",
              result$gradient_norm))
  cat("Valores propios de la Hessiana escalada:",
      paste(sprintf("%.8f", eigenvalues), collapse = ", "), "\n")
  if (all(eigenvalues > 0)) {
    cat("La Hessiana es positiva definida: mínimo local estricto.\n")
  }
}

# Punto inicial del ejercicio, expresado en las coordenadas escaladas.
z0 = c(tau = 0.5, kappa = 1)
newton_result = newton_method(z0)
bfgs_result = bfgs_method(z0)

report_result("Newton", newton_result)
report_result("BFGS", bfgs_result)

