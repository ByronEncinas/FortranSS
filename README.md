# FortranST (Library for Numerical Methods)

The idea is that all the results from the method that take large population samples as input will be compatible with the input of all the Plotting functions as well.

- [x] **Object Particle**: initialize(mass, init_pos, init_vel, charge, ...), displacement updates method
- [ ] **Next Features in Object Particle**: Electric Field, Grav Field created by it.

- [x] **Object Calculus**: 
  - First Derivative, Second Derivative (Done)
  - Euler Method (Done)
  - Simpson's Method (1/3, 1/8, 3/8)
  pending...
  - Crank-Nicholson
  - Finite difference approximations
  - FFT

- [ ] **Next Features in Object Calculus**: 
  - Symplectic Integrator Feature (Predictor-Corrector, etc. for all previous methods)
  - **Numerical Methods for ODE and PDE:**
    - Implicit and Explicit Methods (Euler, RK4, etc.)
    - Adaptive step-size methods (Runge-Kutta-Fehlberg, Dormand-Prince)
    - Finite Volume Methods (FVM)
    - Finite Element Methods (FEM)
    - Spectral methods
    - Stability, consistency, and convergence analysis (Von Neumann stability, CFL condition)

- [ ] **Object Matrix**: 
  - Initialize Levi-Civita Tensor
  - Square matrices methods
  - Penrose inverse matrix
  - Gauss-Jordan Method
  - Trace of Matrix
  - Determinant
  - Inverse
  - LU Decomposition

- [ ] **Linear Regression and Interpolation**: 
  - Interpolation of N-grade polynomials
  - Least squares fitting and regression

- [ ] **Special Functions**:
  - Logarithmic Integral
  - Exponential Integral
  - Error Function
  - Elliptic Functions (all types)


