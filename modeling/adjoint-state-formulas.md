
# Beyond machine learning: physicaly constrained optimization
    
I explain how to add implicit equations to an optimization problem. I will also show an example of fitting model to data
where the model and the observed data constrained with an implicit equation.


```python
from IPython.display import display, Math, Latex
```


```python
display(Math(r"""\begin{equation}\mathrel{\text{Constrained optimization: explicit vs. implicit}}\end{equation}"""))
```


$$\begin{equation}\mathrel{\text{Constrained optimization: explicit vs. implicit}}\end{equation}$$



```python
##Constrained optimization: explicit vs. implicit
```

\begin{equation}
\mathrel{\text{min}} L(Y,m) 
\mathrel{\text{subject to:}} Y=Y(m) \rightarrow 
\frac{\partial L}{\partial m} = \frac{\partial L}{\partial Y} \frac{\partial Y}{\partial m} 
\end{equation}

\begin{equation}
\mathrel{\text{min}} L(Y,m) 
\mathrel{\text{subject to:}} F(Y,m)=0 \rightarrow \frac{\partial L}{\partial m}=?
\end{equation}


```python
display(Math(r"""\begin{equation}\mathrel{\text{Adjoin State Method for Constrained Optimization}}\end{equation}"""))
```


$$\begin{equation}\mathrel{\text{Adjoin State Method for Constrained Optimization}}\end{equation}$$


Adjoint state method overview: start with two solutions $(Y,m)$ and $(Y+\delta Y, m + \delta m)$ satisfying $F = 0$:

\begin{equation}
F(Y+\delta Y, m + \delta m) = F(Y,m) + \frac{\partial F}{\partial Y}\delta Y + \frac{\partial F}{\partial m}\delta m 
\rightarrow
\frac{\partial F}{\partial Y}\delta Y = - \frac{\partial F}{\partial m}\delta m
\rightarrow
\delta Y = - \left( \frac{\partial F}{\partial Y} \right )^{-1} \frac{\partial F}{\partial m}\delta m
\end{equation}

Find optimization gradient as follow:

\begin{equation}
\frac{\partial L}{\partial m} = \frac{\partial L}{\partial Y} \frac{\partial Y}{\partial m} = 
-\frac{\partial L}{\partial Y} \left (  \frac{\partial F}{\partial Y} \right )^{-1}\frac{\partial F}{\partial m} =-
\left [ \left (  \frac{\partial F}{\partial Y} \right )^{-1} \right ]^* 
\frac{\partial L}{\partial Y} 
\frac{\partial F}{\partial m}=
-\left [ \left (  \frac{\partial F}{\partial Y} \right )^* \right ]^{-1} 
\frac{\partial L}{\partial Y} \frac{\partial F}{\partial m}=
-\lambda \frac{\partial F}{\partial m}
\\
\mathrel{\text{where}} \lambda \mathrel{\text{defined as solution to adjoint problem:}}
\left (  \frac{\partial F}{\partial Y} \right )^* \lambda = \frac{\partial L}{\partial Y}
\end{equation}

Example

Fitting data using hyperbolic non-parametric quation:

\begin{equation}
\mathrel{\text{min}} L(Y,m) \mathrel{\text{subject to:}} F(Y,m)=0  \\
\mathrel{\text{where}} L = \int (Y-Y^{obs})^2dx \\
\mathrel{\text{subject to:}} -Y^2 + x^2 -m^2 = 0
\end{equation}

Find the adjoint state equations: 

\begin{equation}
\frac{\partial L}{\partial Y} = Y-Y^{obs} \\
\frac{\partial F}{\partial m} = -2m  \\
\frac{\partial F}{\partial Y} = -2Y \rightarrow \left ( \frac{\partial F}{\partial Y} \right )^*=-2Y
\end{equation}

First solve for $\lambda$ as follows:

\begin{equation}
\left (  \frac{\partial F}{\partial Y} \right )^{*} \lambda = \frac{\partial L}{\partial Y} \rightarrow
-2Y\lambda =  Y-Y^{obs} \rightarrow \lambda = -\frac{Y-Y^{obs}}{2Y}
\end{equation}

Now find the solution gradient:

\begin{equation}
\frac{\partial L}{\partial m} = 
-\left [ \left (  \frac{\partial F}{\partial Y} \right )^{*} \right ]^{-1} \frac{\partial L}{\partial Y} \frac{\partial F}{\partial m} 
= - \lambda \frac{\partial F}{\partial m}
= - \int \lambda (-2m) 
= 2m \int \lambda 
= 2m \int -\frac{Y-Y^{obs}}{2Y}
\end{equation}

\begin{equation}
\mathrel{\text{Gradient descent:}} 
m \leftarrow m - \alpha \frac{\partial L}{\partial m}
\end{equation}
