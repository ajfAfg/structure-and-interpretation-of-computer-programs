# 練習問題 1.13

## 補題1: $\mathit{Fib}(n) = \frac{\phi^n - \psi^n}{\sqrt{5}}$

$n$ について場合分けして考える。

### $n = 0$ の場合

$$
\frac{\phi^0 - \psi^0}{\sqrt{5}} = 0 = \mathit{Fib}(0)
$$

### $n = 1$ の場合

$$
\begin{aligned}
\frac{\phi^1 - \psi^1}{\sqrt{5}} &= \frac{\frac{1+\sqrt{5}}{2} - \frac{1-\sqrt{5}}{2}}{\sqrt{5}} \\
                                 &= 1 \\
                                 &= \mathit{Fib}(1)
\end{aligned}
$$

### $n > 1$ の場合

$n$ に関する完全帰納法で示す。

$$
\begin{aligned}
\mathit{Fib}(n) &= \mathit{Fib}(n-1) + \mathit{Fib}(n-2) \\
                &= \frac{\phi^{n-1} - \psi^{n-1}}{\sqrt{5}} + \frac{\phi^{n-2} - \psi^{n-2}}{\sqrt{5}} \quad (\text{帰納法の仮定による})\\
                &= \frac{\phi^n - \psi^n}{\sqrt{5}} + \frac{(\phi^{n-2} + \phi^{n-1} - \phi^n) - (\psi^{n-2} + \psi^{n-1} - \psi^n)}{\sqrt{5}} \\
                &= \frac{\phi^n - \psi^n}{\sqrt{5}}
\end{aligned}
$$

ただし、

$$
\begin{aligned}
(\phi^{n-2} + \phi^{n-1} - \phi^n) - (\psi^{n-2} + \psi^{n-1} - \psi^n)
    &= \phi^{n-2}(1 + \phi - \phi^2) - \psi^{n-2}(1 + \psi - \psi^2) \\
    &= \phi^{n-2}(1 + \frac{1 + \sqrt{5}}{2} - \frac{3 + \sqrt{5}}{2}) - \psi^{n-2}(1 + \frac{1 - \sqrt{5}}{2} - \frac{3 - \sqrt{5}}{2}) \\
    &= 0
\end{aligned}
$$

## 定理: $\left|\mathit{Fib}(n) - \frac{\phi^n}{\sqrt{5}}\right| < \frac{1}{2}$

$|\psi| = \frac{\sqrt{5} - 1}{2} < 1$ だから $\left|\frac{\psi^n}{\sqrt{5}}\right| \le \frac{1}{\sqrt{5}} < \frac{1}{2}$ が成り立つ。したがって、

$$
\begin{aligned}
\left|\mathit{Fib}(n) - \frac{\phi^n}{\sqrt{5}}\right| &= \left| \frac{\phi^n - \psi^n}{\sqrt{5}} - \frac{\phi^n}{\sqrt{5}} \right| \quad (\text{補題1による}) \\
    &= \left|\frac{\psi^n}{\sqrt{5}}\right| \\
    &< \frac{1}{2}
\end{aligned}
$$
