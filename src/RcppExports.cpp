// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// compute_split_frequencies
Rcpp::NumericMatrix compute_split_frequencies(Rcpp::List forest_object, size_t max_depth);
RcppExport SEXP _grf_compute_split_frequencies(SEXP forest_objectSEXP, SEXP max_depthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< size_t >::type max_depth(max_depthSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_split_frequencies(forest_object, max_depth));
    return rcpp_result_gen;
END_RCPP
}
// compute_weights
Eigen::SparseMatrix<double> compute_weights(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_compute_weights(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_weights(forest_object, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// compute_weights_oob
Eigen::SparseMatrix<double> compute_weights_oob(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_compute_weights_oob(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_weights_oob(forest_object, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// deserialize_tree
Rcpp::List deserialize_tree(Rcpp::List forest_object, size_t tree_index);
RcppExport SEXP _grf_deserialize_tree(SEXP forest_objectSEXP, SEXP tree_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< size_t >::type tree_index(tree_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(deserialize_tree(forest_object, tree_index));
    return rcpp_result_gen;
END_RCPP
}
// custom_train
Rcpp::List custom_train(Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, size_t outcome_index, unsigned int mtry, unsigned int num_trees, unsigned int num_threads, unsigned int min_node_size, double sample_fraction, unsigned int seed, bool honesty, unsigned int ci_group_size, double alpha, double imbalance_penalty, std::vector<size_t> clusters, unsigned int samples_per_cluster);
RcppExport SEXP _grf_custom_train(SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP outcome_indexSEXP, SEXP mtrySEXP, SEXP num_treesSEXP, SEXP num_threadsSEXP, SEXP min_node_sizeSEXP, SEXP sample_fractionSEXP, SEXP seedSEXP, SEXP honestySEXP, SEXP ci_group_sizeSEXP, SEXP alphaSEXP, SEXP imbalance_penaltySEXP, SEXP clustersSEXP, SEXP samples_per_clusterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< size_t >::type outcome_index(outcome_indexSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_trees(num_treesSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type sample_fraction(sample_fractionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< bool >::type honesty(honestySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type imbalance_penalty(imbalance_penaltySEXP);
    Rcpp::traits::input_parameter< std::vector<size_t> >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type samples_per_cluster(samples_per_clusterSEXP);
    rcpp_result_gen = Rcpp::wrap(custom_train(input_data, sparse_input_data, outcome_index, mtry, num_trees, num_threads, min_node_size, sample_fraction, seed, honesty, ci_group_size, alpha, imbalance_penalty, clusters, samples_per_cluster));
    return rcpp_result_gen;
END_RCPP
}
// custom_predict
Rcpp::NumericMatrix custom_predict(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_custom_predict(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(custom_predict(forest_object, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// custom_predict_oob
Rcpp::NumericMatrix custom_predict_oob(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_custom_predict_oob(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(custom_predict_oob(forest_object, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// instrumental_train
Rcpp::List instrumental_train(Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, size_t outcome_index, size_t treatment_index, size_t instrument_index, unsigned int mtry, unsigned int num_trees, unsigned int num_threads, unsigned int min_node_size, double sample_fraction, unsigned int seed, bool honesty, unsigned int ci_group_size, double reduced_form_weight, double alpha, bool imbalance_penalty, bool stabilize_splits, std::vector<size_t> clusters, unsigned int samples_per_cluster);
RcppExport SEXP _grf_instrumental_train(SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP outcome_indexSEXP, SEXP treatment_indexSEXP, SEXP instrument_indexSEXP, SEXP mtrySEXP, SEXP num_treesSEXP, SEXP num_threadsSEXP, SEXP min_node_sizeSEXP, SEXP sample_fractionSEXP, SEXP seedSEXP, SEXP honestySEXP, SEXP ci_group_sizeSEXP, SEXP reduced_form_weightSEXP, SEXP alphaSEXP, SEXP imbalance_penaltySEXP, SEXP stabilize_splitsSEXP, SEXP clustersSEXP, SEXP samples_per_clusterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< size_t >::type outcome_index(outcome_indexSEXP);
    Rcpp::traits::input_parameter< size_t >::type treatment_index(treatment_indexSEXP);
    Rcpp::traits::input_parameter< size_t >::type instrument_index(instrument_indexSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_trees(num_treesSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type sample_fraction(sample_fractionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< bool >::type honesty(honestySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type reduced_form_weight(reduced_form_weightSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type imbalance_penalty(imbalance_penaltySEXP);
    Rcpp::traits::input_parameter< bool >::type stabilize_splits(stabilize_splitsSEXP);
    Rcpp::traits::input_parameter< std::vector<size_t> >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type samples_per_cluster(samples_per_clusterSEXP);
    rcpp_result_gen = Rcpp::wrap(instrumental_train(input_data, sparse_input_data, outcome_index, treatment_index, instrument_index, mtry, num_trees, num_threads, min_node_size, sample_fraction, seed, honesty, ci_group_size, reduced_form_weight, alpha, imbalance_penalty, stabilize_splits, clusters, samples_per_cluster));
    return rcpp_result_gen;
END_RCPP
}
// instrumental_predict
Rcpp::List instrumental_predict(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads, unsigned int ci_group_size);
RcppExport SEXP _grf_instrumental_predict(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP, SEXP ci_group_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(instrumental_predict(forest_object, input_data, sparse_input_data, num_threads, ci_group_size));
    return rcpp_result_gen;
END_RCPP
}
// instrumental_predict_oob
Rcpp::List instrumental_predict_oob(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads, unsigned int ci_group_size);
RcppExport SEXP _grf_instrumental_predict_oob(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP, SEXP ci_group_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(instrumental_predict_oob(forest_object, input_data, sparse_input_data, num_threads, ci_group_size));
    return rcpp_result_gen;
END_RCPP
}
// quantile_train
Rcpp::List quantile_train(std::vector<double> quantiles, bool regression_splits, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, size_t outcome_index, unsigned int mtry, unsigned int num_trees, int num_threads, int min_node_size, double sample_fraction, unsigned int seed, bool honesty, unsigned int ci_group_size, double alpha, double imbalance_penalty, std::vector<size_t> clusters, unsigned int samples_per_cluster);
RcppExport SEXP _grf_quantile_train(SEXP quantilesSEXP, SEXP regression_splitsSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP outcome_indexSEXP, SEXP mtrySEXP, SEXP num_treesSEXP, SEXP num_threadsSEXP, SEXP min_node_sizeSEXP, SEXP sample_fractionSEXP, SEXP seedSEXP, SEXP honestySEXP, SEXP ci_group_sizeSEXP, SEXP alphaSEXP, SEXP imbalance_penaltySEXP, SEXP clustersSEXP, SEXP samples_per_clusterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type quantiles(quantilesSEXP);
    Rcpp::traits::input_parameter< bool >::type regression_splits(regression_splitsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< size_t >::type outcome_index(outcome_indexSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_trees(num_treesSEXP);
    Rcpp::traits::input_parameter< int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< int >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type sample_fraction(sample_fractionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< bool >::type honesty(honestySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type imbalance_penalty(imbalance_penaltySEXP);
    Rcpp::traits::input_parameter< std::vector<size_t> >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type samples_per_cluster(samples_per_clusterSEXP);
    rcpp_result_gen = Rcpp::wrap(quantile_train(quantiles, regression_splits, input_data, sparse_input_data, outcome_index, mtry, num_trees, num_threads, min_node_size, sample_fraction, seed, honesty, ci_group_size, alpha, imbalance_penalty, clusters, samples_per_cluster));
    return rcpp_result_gen;
END_RCPP
}
// quantile_predict
Rcpp::NumericMatrix quantile_predict(Rcpp::List forest_object, std::vector<double> quantiles, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_quantile_predict(SEXP forest_objectSEXP, SEXP quantilesSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type quantiles(quantilesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(quantile_predict(forest_object, quantiles, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// quantile_predict_oob
Rcpp::NumericMatrix quantile_predict_oob(Rcpp::List forest_object, std::vector<double> quantiles, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads);
RcppExport SEXP _grf_quantile_predict_oob(SEXP forest_objectSEXP, SEXP quantilesSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type quantiles(quantilesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(quantile_predict_oob(forest_object, quantiles, input_data, sparse_input_data, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// regression_train
Rcpp::List regression_train(Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, size_t outcome_index, unsigned int mtry, unsigned int num_trees, unsigned int num_threads, unsigned int min_node_size, double sample_fraction, unsigned int seed, bool honesty, unsigned int ci_group_size, double alpha, double imbalance_penalty, std::vector<size_t> clusters, unsigned int samples_per_cluster);
RcppExport SEXP _grf_regression_train(SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP outcome_indexSEXP, SEXP mtrySEXP, SEXP num_treesSEXP, SEXP num_threadsSEXP, SEXP min_node_sizeSEXP, SEXP sample_fractionSEXP, SEXP seedSEXP, SEXP honestySEXP, SEXP ci_group_sizeSEXP, SEXP alphaSEXP, SEXP imbalance_penaltySEXP, SEXP clustersSEXP, SEXP samples_per_clusterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< size_t >::type outcome_index(outcome_indexSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_trees(num_treesSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type sample_fraction(sample_fractionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< bool >::type honesty(honestySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type imbalance_penalty(imbalance_penaltySEXP);
    Rcpp::traits::input_parameter< std::vector<size_t> >::type clusters(clustersSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type samples_per_cluster(samples_per_clusterSEXP);
    rcpp_result_gen = Rcpp::wrap(regression_train(input_data, sparse_input_data, outcome_index, mtry, num_trees, num_threads, min_node_size, sample_fraction, seed, honesty, ci_group_size, alpha, imbalance_penalty, clusters, samples_per_cluster));
    return rcpp_result_gen;
END_RCPP
}
// regression_predict
Rcpp::List regression_predict(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads, unsigned int ci_group_size);
RcppExport SEXP _grf_regression_predict(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP, SEXP ci_group_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(regression_predict(forest_object, input_data, sparse_input_data, num_threads, ci_group_size));
    return rcpp_result_gen;
END_RCPP
}
// regression_predict_oob
Rcpp::List regression_predict_oob(Rcpp::List forest_object, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, unsigned int num_threads, unsigned int ci_group_size);
RcppExport SEXP _grf_regression_predict_oob(SEXP forest_objectSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP num_threadsSEXP, SEXP ci_group_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest_object(forest_objectSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type ci_group_size(ci_group_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(regression_predict_oob(forest_object, input_data, sparse_input_data, num_threads, ci_group_size));
    return rcpp_result_gen;
END_RCPP
}
// local_linear_predict
Rcpp::List local_linear_predict(Rcpp::List forest, Rcpp::NumericMatrix input_data, Rcpp::NumericMatrix training_data, Eigen::SparseMatrix<double> sparse_input_data, Eigen::SparseMatrix<double> sparse_training_data, double lambda, bool use_unweighted_penalty, unsigned int num_threads);
RcppExport SEXP _grf_local_linear_predict(SEXP forestSEXP, SEXP input_dataSEXP, SEXP training_dataSEXP, SEXP sparse_input_dataSEXP, SEXP sparse_training_dataSEXP, SEXP lambdaSEXP, SEXP use_unweighted_penaltySEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type training_data(training_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_training_data(sparse_training_dataSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type use_unweighted_penalty(use_unweighted_penaltySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(local_linear_predict(forest, input_data, training_data, sparse_input_data, sparse_training_data, lambda, use_unweighted_penalty, num_threads));
    return rcpp_result_gen;
END_RCPP
}
// local_linear_predict_oob
Rcpp::List local_linear_predict_oob(Rcpp::List forest, Rcpp::NumericMatrix input_data, Eigen::SparseMatrix<double> sparse_input_data, double lambda, bool use_unweighted_penalty, unsigned int num_threads);
RcppExport SEXP _grf_local_linear_predict_oob(SEXP forestSEXP, SEXP input_dataSEXP, SEXP sparse_input_dataSEXP, SEXP lambdaSEXP, SEXP use_unweighted_penaltySEXP, SEXP num_threadsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< Eigen::SparseMatrix<double> >::type sparse_input_data(sparse_input_dataSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< bool >::type use_unweighted_penalty(use_unweighted_penaltySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type num_threads(num_threadsSEXP);
    rcpp_result_gen = Rcpp::wrap(local_linear_predict_oob(forest, input_data, sparse_input_data, lambda, use_unweighted_penalty, num_threads));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_grf_compute_split_frequencies", (DL_FUNC) &_grf_compute_split_frequencies, 2},
    {"_grf_compute_weights", (DL_FUNC) &_grf_compute_weights, 4},
    {"_grf_compute_weights_oob", (DL_FUNC) &_grf_compute_weights_oob, 4},
    {"_grf_deserialize_tree", (DL_FUNC) &_grf_deserialize_tree, 2},
    {"_grf_custom_train", (DL_FUNC) &_grf_custom_train, 15},
    {"_grf_custom_predict", (DL_FUNC) &_grf_custom_predict, 4},
    {"_grf_custom_predict_oob", (DL_FUNC) &_grf_custom_predict_oob, 4},
    {"_grf_instrumental_train", (DL_FUNC) &_grf_instrumental_train, 19},
    {"_grf_instrumental_predict", (DL_FUNC) &_grf_instrumental_predict, 5},
    {"_grf_instrumental_predict_oob", (DL_FUNC) &_grf_instrumental_predict_oob, 5},
    {"_grf_quantile_train", (DL_FUNC) &_grf_quantile_train, 17},
    {"_grf_quantile_predict", (DL_FUNC) &_grf_quantile_predict, 5},
    {"_grf_quantile_predict_oob", (DL_FUNC) &_grf_quantile_predict_oob, 5},
    {"_grf_regression_train", (DL_FUNC) &_grf_regression_train, 15},
    {"_grf_regression_predict", (DL_FUNC) &_grf_regression_predict, 5},
    {"_grf_regression_predict_oob", (DL_FUNC) &_grf_regression_predict_oob, 5},
    {"_grf_local_linear_predict", (DL_FUNC) &_grf_local_linear_predict, 8},
    {"_grf_local_linear_predict_oob", (DL_FUNC) &_grf_local_linear_predict_oob, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_grf(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
