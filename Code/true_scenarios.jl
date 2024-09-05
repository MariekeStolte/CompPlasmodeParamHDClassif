using Bigsimr
using Distributions
using LinearAlgebra
using Plots
using Random
using StatsBase

Random.seed!(2601)
normal_means = [rand(Normal(0, 2)) for i in 1:50]
normal_sds = [rand(InverseGamma(15, 14)) for i in 1:50]
log_normal_means = [rand(Normal(0, 2)) for i in 1:50]
log_normal_sds = [rand(InverseGamma(15,14)) for i in 1:50]

normal_mixture_means = vcat([[rand(Normal(0, 1)), rand(Normal(0, 1))] for i in 1:25],
                            [[rand(Normal(0, 1)), rand(Normal(4, 1))] for i in 1:25])
normal_mixture_sds = vcat([[rand(InverseGamma(3, 2)), rand(InverseGamma(2.5,15))] for i in 1:25],
                          [[rand(InverseGamma(3, 2)), rand(InverseGamma(3, 2))] for i in 1:25])
normal_mixture_alphas = [rand(Uniform(0.05, 0.95)) for i in 1:50]

margins = vec(vcat(Normal.(normal_means, normal_sds),
                LogNormal.(log_normal_means, log_normal_sds),
                UnivariateGMM.(normal_mixture_means, normal_mixture_sds,
                                mapslices(Categorical, hcat(normal_mixture_alphas, 1 .- normal_mixture_alphas); dims = 2))))

prior_cov = fill(0.0, 150, 150); prior_cov[diagind(prior_cov)] .= 1.0;
for i in eachindex(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        bounds = pearson_bounds(margins[i], margins[j])
        prior_cov[i,j] = prior_cov[j,i] = mean(bounds)
    end
end
prior_cov = cor_nearPD(prior_cov)
Random.seed!(2601)
nu = 250
corr_dist = InverseWishart(nu, prior_cov * (nu - 151))
target_corr = rand(corr_dist)
target_corr = cor_nearPD(target_corr)

histogram([target_corr[i, j] for j in axes(target_corr, 1) for i in 1:(j-1)], normalize=:pdf)


res = zeros(150, 150)
for i in eachindex(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(target_corr[i, j], margins[i], margins[j])
    end
end
problems = findall(isnan.(res))

adjusted_corr = pearson_match(target_corr, margins)

# Correlation
new_target_corr = fill(0.0, 150, 150); new_target_corr[diagind(new_target_corr)] .= 1.0;
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))

new_target_corr = fill(0.0, 150, 150); new_target_corr[diagind(new_target_corr)] .= 1.0;
Random.seed!(2002)
counter = 0
for i in 1:100
    means = [rand(Normal(0, 1)), rand(Normal(4, 1))]
    try
        if isnan(pearson_match(new_target_corr[19, 143], margins[19], UnivariateGMM(means, std.(components(margins[143])), Categorical(probs(margins[143])))))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr[29, 143], margins[29], UnivariateGMM(means, std.(components(margins[143])), Categorical(probs(margins[143])))))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[143] = UnivariateGMM(means, std.(components(margins[143])), Categorical(probs(margins[143])))
    break
end
pearson_match(new_target_corr[29, 143], margins[29], margins[143])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adj_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)


new_target_corr = fill(0.2, 150, 150); new_target_corr[diagind(new_target_corr)] .= 1.0;
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))
margins[[59, 75]]
Random.seed!(2002)
counter = 0
for i in 1:100
    sd = rand(InverseGamma(15,14))
    try
        if isnan(pearson_match(new_target_corr[59, 132], LogNormal(mean(margins[59]), sd), margins[132]))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr[59, 140], LogNormal(mean(margins[59]), sd), margins[140]))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr[59, 142], LogNormal(mean(margins[59]), sd), margins[142]))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[59] = LogNormal(mean(margins[59]), sd)
    break
end
pearson_match(new_target_corr[59, 142], margins[59], margins[142])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adj_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))
Random.seed!(2102)
counter = 0
for i in 1:100
    sd = rand(InverseGamma(15,14))
    try
        if isnan(pearson_match(new_target_corr[75, 142], LogNormal(mean(margins[75]), sd), margins[142]))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[75] = LogNormal(mean(margins[75]), sd)
    break
end
pearson_match(new_target_corr[75, 142], margins[75], margins[142])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adj_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr_1)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)
new_target_corr_2 = fill(0.2, 150, 150); new_target_corr_2[diagind(new_target_corr_2)] .= 1.0;
adj_corr_2 = pearson_match(new_target_corr_2, margins)

new_target_corr_min_1 = fill(-0.1, 150, 150); new_target_corr_min_1[diagind(new_target_corr_min_1)] .= 1.0;
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr_min_1[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))
Random.seed!(0803)
counter = 0;
for i in 1:100
    means = [rand(Normal(0, 1)), rand(Normal(4, 1))]
    try
        if isnan(pearson_match(new_target_corr_min_1[29, 140], margins[29], UnivariateGMM(means, std.(components(margins[140])), Categorical(probs(margins[140])))))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[140] = UnivariateGMM(means, std.(components(margins[140])), Categorical(probs(margins[140])))
    break
end
pearson_match(new_target_corr[29, 140], margins[28], margins[140])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adj_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr_1)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)
new_target_corr_2 = fill(0.2, 150, 150); new_target_corr_2[diagind(new_target_corr_2)] .= 1.0;
adj_corr_2 = pearson_match(new_target_corr_2, margins)
new_target_corr_min_1 = fill(-0.1, 150, 150); new_target_corr_min_1[diagind(new_target_corr_min_1)] .= 1.0;
adj_corr_min_1 = pearson_match(new_target_corr_min_1, margins)

new_target_corr_min_2 = fill(-0.2, 150, 150); new_target_corr_min_2[diagind(new_target_corr_min_2)] .= 1.0;
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr_min_2[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))
Random.seed!(0903)
counter = 0;
for i in 1:100
    sd = rand(InverseGamma(15,14))
    try
        if isnan(pearson_match(new_target_corr_min_2[51, 98], margins[51], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[54, 98], margins[54], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[55, 98], margins[55], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[56, 98], margins[56], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[57, 98], margins[57], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[59, 98], margins[59], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[61, 98], margins[61], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[62, 98], margins[62], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[65, 98], margins[65], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[67, 98], margins[67], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[68, 98], margins[68], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[80, 98], margins[80], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[86, 98], margins[86], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[91, 98], margins[91], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[93, 98], margins[93], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[97, 98], margins[97], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[100, 98], margins[100], LogNormal(mean(margins[98]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[98] = LogNormal(mean(margins[98]), sd)
    break
end
pearson_match(new_target_corr_min_2[98, 100], margins[98], margins[100])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adj_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr_1)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)
new_target_corr_2 = fill(0.2, 150, 150); new_target_corr_2[diagind(new_target_corr_2)] .= 1.0;
adj_corr_2 = pearson_match(new_target_corr_2, margins)
new_target_corr_min_1 = fill(-0.1, 150, 150); new_target_corr_min_1[diagind(new_target_corr_min_1)] .= 1.0;
adj_corr_min_1 = pearson_match(new_target_corr_min_1, margins)
new_target_corr_min_2 = fill(-0.2, 150, 150); new_target_corr_min_2[diagind(new_target_corr_min_2)] .= 1.0;
res = zeros(150, 150);
for i in 1:length(margins)
    for j in (i+1):length(margins)
        println("i = $(i), j = $(j)")
        res[i,j] = pearson_match(new_target_corr_min_2[i, j], margins[i], margins[j])
    end
end
findall(isnan.(res))
Random.seed!(1003)
counter = 0;
for i in 1:100
    sd = rand(InverseGamma(15,14))
    try
        if isnan(pearson_match(new_target_corr_min_2[57, 100], margins[57], LogNormal(mean(margins[100]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    try
        if isnan(pearson_match(new_target_corr_min_2[65, 100], margins[65], LogNormal(mean(margins[100]), sd)))
            counter = counter + 1
            continue
        end
    catch
        counter = counter + 1
        continue
    end
    margins[100] = LogNormal(mean(margins[100]), sd)
    break
end
pearson_match(new_target_corr_min_2[98, 100], margins[98], margins[100])
no_corr = fill(0.0, 150, 150); no_corr[diagind(no_corr)] .= 1.0;
adj_no_corr = pearson_match(no_corr, margins)
adjusted_corr = pearson_match(target_corr, margins)
new_target_corr_1 = fill(0.1, 150, 150); new_target_corr_1[diagind(new_target_corr_1)] .= 1.0;
adj_corr_1 = pearson_match(new_target_corr_1, margins)
new_target_corr_2 = fill(0.2, 150, 150); new_target_corr_2[diagind(new_target_corr_2)] .= 1.0;
adj_corr_2 = pearson_match(new_target_corr_2, margins)
new_target_corr_min_1 = fill(-0.1, 150, 150); new_target_corr_min_1[diagind(new_target_corr_min_1)] .= 1.0;
adj_corr_min_1 = pearson_match(new_target_corr_min_1, margins)
new_target_corr_min_2 = fill(-0.2, 150, 150); new_target_corr_min_2[diagind(new_target_corr_min_2)] .= 1.0;
adj_corr_min_2 = pearson_match(new_target_corr_min_2, margins)

using CSV
using DataFrames
mar_df = DataFrame(margin = margins)
CSV.write("..\\Results\\true_dgp_margins.csv", mar_df, newline = "___")
CSV.write("..\\Results\\true_dgp_adj_cor.csv", adjusted_corr)
CSV.write("..\\Results\\true_dgp_target_cor.csv", DataFrame(target_corr, :auto))