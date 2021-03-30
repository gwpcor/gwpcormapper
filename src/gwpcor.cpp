//
// Created by Joseph Percival on 2021/02/20.
//

#include <Rcpp.h>

using namespace Rcpp;

/*
 * Gaussian Kernel
 */
NumericVector gaussian(NumericVector v, double b) {
    NumericVector weight = exp((-0.5) * ((pow(v,2))/(pow(b,2))));
    return weight;
}

/*
 * Exponential Kernel
 */
NumericVector exponential(NumericVector v, double b) {
    NumericVector weight = exp(-1 * v/b);
    return weight;
}

/*
 * Bisquare Kernel
 */
NumericVector bisquare(NumericVector v, double b) {
    int l = v.length();
    NumericVector weight(l);
    for (int i = 0; i < l; i++) {
        weight[i] = v[i] > b ? 0 : pow(1 - ((pow(v[i],2))/(pow(b, 2))), 2);
    }
    return weight;
}

/*
 * Tricube Kernel
 */
NumericVector tricube(NumericVector v, double b) {
    int l = v.length();
    NumericVector weight(l);
    for (int i = 0; i < l; i++) {
        weight[i] = v[i] > b ? 0 : pow(1 - (pow(v[i], 3)/pow(b, 3)), 3);
    }
    return weight;
}

/*
 * Boxcar Kernel
 */
NumericVector boxcar(NumericVector v, double b) {
    const int l = v.length();
    NumericVector weight(l);
    for (int i = 0; i < l; i++) {
        weight[i] = v[i] > b ? 0 : 1;
    }
    return weight;
}

/*
 * Spatial weights vector
 */
NumericVector calc_weight(String type, bool adapt, NumericVector dist_vec, double bw ) {
    if (adapt) {
        int l = dist_vec.length();
        int bw_size = l * bw;
        NumericVector y = clone(dist_vec);
        std::sort(y.begin(), y.end());
        bw = y[bw_size - 1];
    }
    NumericVector weight;

    if (type=="gaussian") {
        weight = gaussian(dist_vec, bw);
    }
    else if (type=="exponential") {
        weight = exponential(dist_vec, bw);
    }
    else if (type=="bisquare") {
        weight = bisquare(dist_vec, bw);
    }
    else if (type=="tricube") {
        weight = tricube(dist_vec, bw);
    }
    else if (type=="boxcar") {
        weight = boxcar(dist_vec, bw);
    }
    return weight;
}

/*
 * GW correlation matrix
 */
NumericMatrix cor_wt(const NumericMatrix & P, const NumericVector & Wi) {
    const int n = P.ncol();
    const int m = P.nrow();
    NumericMatrix cor(n, n);

    NumericMatrix X(m, n);
    NumericVector sd(n);
    NumericVector W = Wi / sum(Wi);

    for (int i = 0; i < n; ++i) {
        X(_, i) = (P(_, i) - sum(P(_, i) * W)) * sqrt(W);
        sd(i) = sqrt(sum(X(_, i) * X(_, i)));
        for (int j = 0; j <= i; ++j) {
            cor(i, j) = sum(X(_, i) * X(_, j)) / (sd(i) * sd(j));
            cor(j, i) = cor(i, j);
        }
    }
    return cor;
}

/*
 * P-values for GW correlation
 */
NumericMatrix corrPval(NumericMatrix m, int n) {
    NumericMatrix t(m.nrow(), m.ncol());
    NumericMatrix pValue(m.nrow(), m.ncol());

    for (size_t j = 0; j < m.ncol(); ++j) {
        t(_, j) = m(_, j) * sqrt((n - 2) / (1 - pow(m(_, j), 2)));
        pValue(_, j) = 2 * pt(-abs(t(_, j)), (n - 2));
    }
    pValue.fill_diag(0);
    return pValue;
}

/*
 * P-values for GW partial correlation
 */
NumericMatrix pcorrPval(NumericMatrix m, int n) {
    int gp = m.ncol() - 2;
    NumericMatrix s(m.nrow(), m.ncol());
    NumericMatrix pValue(m.nrow(), m.ncol());

    for (size_t j = 0; j < m.ncol(); ++j) {
        s(_, j) = m(_, j) * sqrt((n - 2 - gp) / (1 - pow(m(_, j), 2)));
        pValue(_, j) = 2 * pt(-abs(s(_, j)), (n - 2 - gp));
    }
    pValue.fill_diag(0);
    return pValue;
}

/*
 * Lower triangle vector
 */
NumericVector lower(NumericMatrix X) {
    int m = X.nrow();
    int n = X.ncol();
    int l = (m * n / 2) - n/2;
    NumericVector v(l);
    int i, j;
    int k = -1;
    for (j = 0; j < n; j++) {
        for (i = 0; i < n; i++) {
            if (j < i) {
                k += 1;
                v[k] = X(i, j);
            }
        }
    }
    return v;
}

/*
 * MAIN FUNCTION
 *
 */
// [[Rcpp::export]]
List gwpcorRcpp(NumericMatrix dMat, double bw, NumericMatrix x,
            String kernel, bool adaptive, int nn) {

    Environment pkg = Environment::namespace_env("corpcor");
    Function cor2pcor = pkg["cor2pcor"];

    int n = dMat.ncol();

    NumericMatrix corrMat(n, nn);
    NumericMatrix corrMat_pval(n, nn);
    NumericMatrix pcorrMat(n, nn);
    NumericMatrix pcorrMat_pval(n, nn);

    for (int j = 0; j < n; ++j) {
        NumericVector Wi = calc_weight(kernel, adaptive, dMat(_, j), bw);
        NumericVector w1 = Wi[Wi != 0];

        int len = w1.length();

        NumericMatrix corrMatrix = cor_wt(x, Wi);
        NumericMatrix pcorrMatrix = cor2pcor(corrMatrix);

        NumericMatrix corrPval_matrix = corrPval(corrMatrix, len);
        NumericMatrix pcorrPval_matrix = pcorrPval(pcorrMatrix, len);

        corrMat(j, _) = lower(corrMatrix);
        corrMat_pval(j, _) = lower(corrPval_matrix);

        pcorrMat(j, _) = lower(pcorrMatrix);
        pcorrMat_pval(j, _) = lower(pcorrPval_matrix);
    }
    List out = List::create(corrMat, corrMat_pval, pcorrMat, pcorrMat_pval);
    return out;
}

