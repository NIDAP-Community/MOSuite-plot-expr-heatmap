#!/usr/bin/env Rscript
rlang::global_entrace()
library(argparse)
library(glue)
library(MOSuite)
library(readr)
library(stringr)
library(dplyr)

# set up results directory
results_dir <- file.path('..','results')
plots_dir <- file.path(results_dir, 'figures')
options(moo_plots_dir = plots_dir, moo_save_plots = TRUE)

# log installed packages & versions
pkg_versions <- tibble::as_tibble(installed.packages())
write_csv(pkg_versions, file.path(results_dir, 'r-packages.csv'))

# parse CLI arguments
parser <- ArgumentParser()

parser$add_argument("--count_type", type="character", default="filt")
parser$add_argument("--sub_count_type", type="character", default=NULL, help="Sub count type if count_type is a list")
parser$add_argument("--sample_id_colname", type="character", default=NULL, help="Column name for sample IDs")
parser$add_argument("--feature_id_colname", type="character", default=NULL, help="Column name for feature IDs")
parser$add_argument("--group_colname", type="character", default="Group", help="Column name for sample groups")
parser$add_argument("--label_colname", type="character", default=NULL, help="Column name for sample labels")
parser$add_argument("--samples_to_include", type="character", default="", help="Comma-separated list of samples to include")
parser$add_argument("--color_values", type="character", default="#5954d6,#e1562c,#b80058,#00c6f8,#d163e6,#00a76c,#ff9287,#008cf9,#006e00,#796880,#FFA500,#878500", help="Comma-separated color values")
parser$add_argument("--include_all_genes", type="logical", default=FALSE, help="Include all genes")
parser$add_argument("--filter_top_genes_by_variance", type="logical", default=TRUE, help="Filter top genes by variance")
parser$add_argument("--top_genes_by_variance_to_include", type="integer", default=500, help="Number of top genes by variance")
parser$add_argument("--specific_genes_to_include_in_heatmap", type="character", default="None", help="Comma-separated list of specific genes")
parser$add_argument("--cluster_genes", type="logical", default=TRUE, help="Cluster genes")
parser$add_argument("--gene_distance_metric", type="character", default="correlation", help="Gene distance metric")
parser$add_argument("--gene_clustering_method", type="character", default="average", help="Gene clustering method")
parser$add_argument("--display_gene_dendrograms", type="logical", default=TRUE, help="Display gene dendrograms")
parser$add_argument("--display_gene_names", type="logical", default=FALSE, help="Display gene names")
parser$add_argument("--center_and_rescale_expression", type="logical", default=TRUE, help="Center and rescale expression")
parser$add_argument("--cluster_samples", type="logical", default=FALSE, help="Cluster samples")
parser$add_argument("--arrange_sample_columns", type="logical", default=TRUE, help="Arrange sample columns")
parser$add_argument("--order_by_gene_expression", type="logical", default=FALSE, help="Order by gene expression")
parser$add_argument("--gene_to_order_columns", type="character", default=" ", help="Gene to order columns")
parser$add_argument("--gene_expression_order", type="character", default="low_to_high", help="Gene expression order")
parser$add_argument("--smpl_distance_metric", type="character", default="correlation", help="Sample distance metric")
parser$add_argument("--smpl_clustering_method", type="character", default="average", help="Sample clustering method")
parser$add_argument("--display_smpl_dendrograms", type="logical", default=TRUE, help="Display sample dendrograms")
parser$add_argument("--reorder_dendrogram", type="logical", default=FALSE, help="Reorder dendrogram")
parser$add_argument("--reorder_dendrogram_order", type="character", default="", help="Reorder dendrogram order")
parser$add_argument("--display_sample_names", type="logical", default=TRUE, help="Display sample names")
parser$add_argument("--group_columns", type="character", default="Group,Replicate,Batch", help="Columns for groups")
parser$add_argument("--assign_group_colors", type="logical", default=FALSE, help="Assign group colors")
parser$add_argument("--assign_color_to_sample_groups", type="character", default="", help="Assign color to sample groups")
parser$add_argument("--group_colors", type="character", default="#5954d6,#e1562c,#b80058,#00c6f8,#d163e6,#00a76c,#ff9287,#008cf9,#006e00,#796880,#FFA500,#878500", help="Group colors")
parser$add_argument("--heatmap_color_scheme", type="character", default="Default", help="Heatmap color scheme")
parser$add_argument("--autoscale_heatmap_color", type="logical", default=TRUE, help="Autoscale heatmap color")
parser$add_argument("--set_min_heatmap_color", type="double", default=-2, help="Minimum heatmap color value")
parser$add_argument("--set_max_heatmap_color", type="double", default=2, help="Maximum heatmap color value")
parser$add_argument("--aspect_ratio", type="character", default="Auto", help="Aspect ratio")
parser$add_argument("--legend_font_size", type="integer", default=10, help="Legend font size")
parser$add_argument("--gene_name_font_size", type="integer", default=4, help="Gene name font size")
parser$add_argument("--sample_name_font_size", type="integer", default=8, help="Sample name font size")
parser$add_argument("--display_numbers", type="logical", default=FALSE, help="Display numbers in heatmap")

args <- parser$parse_args()

parse_optional_vector <- function(x) {
    if (is.null(x) || identical(x, "") || length(x) == 0) {
        return(NULL)
    }
    return(trimws(unlist(strsplit(x, ","))))
}

parse_vector_with_default <- function(x, default) {
    parsed <- parse_optional_vector(x)
    if (is.null(parsed)) {
        return(default)
    }
    return(parsed)
}

# validate inputs
regex_moo <- ".*\\.rds$"
data_files <- list.files(file.path('../data'), recursive = TRUE, full.names = TRUE)
moo_files <- Filter(\(x) str_detect(x, regex(regex_moo, ignore_case = TRUE)), data_files)

if (length(moo_files) == 0) {
    stop(glue("No files matching regex: {regex_moo}"))
}
moo_filename <- moo_files[1]
moo <- read_rds(moo_filename)
message(glue('Reading multiOmicDataSet from {moo_filename}'))
if (!inherits(moo, 'MOSuite::multiOmicDataSet')) {
    stop(glue('The input is not a multiOmicDataSet. class: {class(moo)}'))
}

# run MOSuite
plot_expr_heatmap(
    moo,
    count_type = args$count_type,
    sub_count_type = args$sub_count_type,
    sample_id_colname = args$sample_id_colname,
    feature_id_colname = args$feature_id_colname,
    group_colname = args$group_colname,
    label_colname = args$label_colname,
    samples_to_include = parse_optional_vector(args$samples_to_include),
    color_values = parse_optional_vector(args$color_values),
    include_all_genes = args$include_all_genes,
    filter_top_genes_by_variance = args$filter_top_genes_by_variance,
    top_genes_by_variance_to_include = args$top_genes_by_variance_to_include,
    specific_genes_to_include_in_heatmap = parse_vector_with_default(args$specific_genes_to_include_in_heatmap, "None"),
    cluster_genes = args$cluster_genes,
    gene_distance_metric = args$gene_distance_metric,
    gene_clustering_method = args$gene_clustering_method,
    display_gene_dendrograms = args$display_gene_dendrograms,
    display_gene_names = args$display_gene_names,
    center_and_rescale_expression = args$center_and_rescale_expression,
    cluster_samples = args$cluster_samples,
    arrange_sample_columns = args$arrange_sample_columns,
    order_by_gene_expression = args$order_by_gene_expression,
    gene_to_order_columns = args$gene_to_order_columns,
    gene_expression_order = args$gene_expression_order,
    smpl_distance_metric = args$smpl_distance_metric,
    smpl_clustering_method = args$smpl_clustering_method,
    display_smpl_dendrograms = args$display_smpl_dendrograms,
    reorder_dendrogram = args$reorder_dendrogram,
    reorder_dendrogram_order = parse_optional_vector(args$reorder_dendrogram_order),
    display_sample_names = args$display_sample_names,
    group_columns = parse_optional_vector(args$group_columns),
    assign_group_colors = args$assign_group_colors,
    assign_color_to_sample_groups = parse_optional_vector(args$assign_color_to_sample_groups),
    group_colors = parse_optional_vector(args$group_colors)
)
