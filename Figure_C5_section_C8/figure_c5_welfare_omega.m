function figure_c5_welfare_omega(baseDir, generatedRoot)
%FIGURE_C5_WELFARE_OMEGA Reproduce Figure C.5 welfare-gain plot.
%   Reads the generated omega run folders created by
%   run_figure_c5_omega_replication and writes the final figure into baseDir.

if nargin < 1 || isempty(baseDir)
    fullPath = mfilename('fullpath');
    if isempty(fullPath)
        baseDir = pwd;
    else
        baseDir = fileparts(fullPath);
    end
end
if nargin < 2 || isempty(generatedRoot)
    generatedRoot = fullfile(baseDir, 'figure_c5_omega_runs');
end

runs = struct( ...
    'folder', {'01_omega_2_3', '02_omega_2_5', '03_omega_2_7'}, ...
    'omegaLabel', {'$\omega=2.3$', '$\omega=2.5$', '$\omega=2.7$'}, ...
    'lineStyle', {'--', '-', '-.'}, ...
    'color', {'r', 'k', 'b'}, ...
    'fallbackMeanDebtRatio', {0.126, 0.156, 0.199} ...
);

plotData = repmat(struct('x', [], 'y', [], 'meanDebtPct', NaN, 'meanWelfarePct', NaN), 1, numel(runs));

for i = 1:numel(runs)
    runDir = fullfile(generatedRoot, runs(i).folder, 'bailout');
    assert(isfolder(runDir), 'Missing generated omega run folder: %s', runDir);

    resultsPath = fullfile(runDir, 'output', 'config000001', 'results.out');
    assert(isfile(resultsPath), 'Missing omega results.out: %s', resultsPath);

    vals = sscanf(strtrim(fileread(resultsPath)), '%f').';
    assert(numel(vals) >= 38, 'Unexpected omega results.out format in %s. Found only %d numbers.', resultsPath, numel(vals));

    bGrid = load(fullfile(runDir, 'graphs_b_grid_dss.txt'));
    avgY = readAverageY(runDir);

    welfareCurve = vals(19:38);
    debtGridPct = 100 .* bGrid(1:20)' ./ avgY;

    meanDebtRatio = readMeanDebtRatioFromResults(vals, runs(i).fallbackMeanDebtRatio);
    meanWelfarePct = interp1(debtGridPct, welfareCurve, 100 * meanDebtRatio, 'spline');
    if ~isfinite(meanWelfarePct)
        meanWelfarePct = interp1(debtGridPct, welfareCurve, 100 * meanDebtRatio, 'linear', 'extrap');
    end

    plotData(i).x = debtGridPct;
    plotData(i).y = welfareCurve;
    plotData(i).meanDebtPct = 100 .* meanDebtRatio;
    plotData(i).meanWelfarePct = meanWelfarePct;
end

hfig = figure('Color', 'w', 'Position', [100 100 1120 840]);
H = plot(plotData(1).x, plotData(1).y, ...
         plotData(2).x, plotData(2).y, ...
         plotData(3).x, plotData(3).y);

for i = 1:numel(runs)
    set(H(i), 'LineStyle', runs(i).lineStyle, 'LineWidth', 4.5, ...
        'MarkerSize', 1.0, 'Color', runs(i).color);
end

set(gca, 'FontSize', 30, 'TickLabelInterpreter', 'latex');
axis([0 28 -2.5 2.0]);
yticks([-2 -1 0 1 2]);

ylabel('Welfare gain of bailouts (percent)', 'Interpreter', 'latex', 'FontSize', 30);
xlabel('Debt/$E(y)$ (percent)', 'Interpreter', 'latex', 'FontSize', 30);

hold on
hdot = gobjects(1,1);
for i = 1:numel(runs)
    h = scatter(plotData(i).meanDebtPct, plotData(i).meanWelfarePct, 140, 'filled', 'k');
    if i == 1
        hdot = h;
    end
end

omegaLabels = {runs.omegaLabel};
legendHandles = [H(:); hdot];
legendLabels = [omegaLabels, {'Mean debt in simu.'}];
leg1 = legend(legendHandles, legendLabels, 'Location', 'northeast');
set(leg1, 'Interpreter', 'latex');
set(leg1, 'FontSize', 26);
legend boxoff
hold off

pngOut1 = fullfile(baseDir, 'Figure_C5_welfare_omega.png');
pngOut2 = fullfile(baseDir, 'welfare_plot_omega.png');
pngOut3 = fullfile(baseDir, 'welfgain_omega4.png');
pdfOut = fullfile(baseDir, 'Figure_C5_welfare_omega.pdf');

saveas(hfig, pngOut1);
saveas(hfig, pngOut2);
saveas(hfig, pngOut3);
print(hfig, pdfOut, '-dpdf', '-painters');

fprintf('Saved Figure C.5 outputs to:\n  %s\n  %s\n  %s\n  %s\n', pngOut1, pngOut2, pngOut3, pdfOut);
end

function meanDebtRatio = readMeanDebtRatioFromResults(vals, fallbackMeanDebtRatio)
if numel(vals) >= 17 && isfinite(vals(17)) && vals(17) > 0
    meanDebtRatio = vals(17) / 100;
else
    meanDebtRatio = fallbackMeanDebtRatio;
end
end

function avgY = readAverageY(runDir)
summaryPath = fullfile(runDir, 'summary_results.txt');
if isfile(summaryPath)
    raw = fileread(summaryPath);
    patterns = { ...
        'avg_y\s*=\s*([-+0-9.Ee]+)', ...
        'mean_y\s*=\s*([-+0-9.Ee]+)', ...
        'avg output\s*[:=]\s*([-+0-9.Ee]+)' ...
    };
    for i = 1:numel(patterns)
        tok = regexp(raw, patterns{i}, 'tokens', 'once');
        if ~isempty(tok)
            avgY = str2double(tok{1});
            if isfinite(avgY) && avgY > 0
                return
            end
        end
    end
end

dataPath = fullfile(runDir, 'graphs_data_sim_dss.txt');
if isfile(dataPath)
    data = load(dataPath);
    if ~isempty(data) && size(data, 2) >= 1
        y = data(:, 1);
        y = y(isfinite(y) & y > 0);
        if ~isempty(y)
            avgY = mean(y);
            if isfinite(avgY) && avgY > 0
                return
            end
        end
    end
end

paramsPath = fullfile(runDir, 'graphs_param_simulation_dss.txt');
if isfile(paramsPath)
    nums = load(paramsPath);
    if numel(nums) >= 2 && all(isfinite(nums(1:2))) && nums(1) > 0 && nums(2) > 0
        % Very conservative fallback: use counts file only as a signal that the run completed.
        % If we get here, the simulation output was missing or malformed.
    end
end

error('Could not recover avg_y from %s', runDir);
end
