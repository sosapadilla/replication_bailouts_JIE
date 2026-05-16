function figure_c6_welfare_theta(baseDir)
%FIGURE_C6_WELFARE_THETA Reproduce Figure C.6 welfare-gain plot.
%  This standalone script reads the theta comparison folders ...
%  and writes the final figure into baseDir.

if nargin < 1 || isempty(baseDir)
    fullPath = mfilename('fullpath');
    if isempty(fullPath)
        baseDir = pwd;
    else
        baseDir = fileparts(fullPath);
    end
end

mainDir = baseDir;

runs = struct( ...
    'folder', {'bailouts-default-theta-0.4', 'bailouts-default-theta-0.5', 'bailouts-default-theta-0.6'}, ...
    'thetaLabel', {'$\theta=0.4$', '$\theta=0.5$', '$\theta=0.6$'}, ...
    'lineStyle', {'--', '-', '-.'}, ...
    'color', {'r', 'k', 'b'}, ...
    'fallbackMeanDebtRatio', {0.245, 0.155, 0.102} ...
);

plotData = repmat(struct('x', [], 'y', [], 'meanDebtPct', NaN, 'meanWelfarePct', NaN), 1, numel(runs));

for i = 1:numel(runs)
    runDir = fullfile(mainDir, runs(i).folder);
    assert(isfolder(runDir), 'Missing folder: %s', runDir);

    welfgain = 100 .* load(fullfile(runDir, 'graphs_welfare_b.txt'));
    avgOutput = load(fullfile(runDir, 'graphs_avg_y.txt'));
    bGrid = load(fullfile(runDir, 'graphs_b_grid_dss.txt'))';
    newgrid = bGrid ./ avgOutput;

    meanDebtRatio = readMeanDebtRatio(runDir, runs(i).fallbackMeanDebtRatio);
    meanWelfarePct = interp1(newgrid, welfgain(1,:), meanDebtRatio, 'spline');

    plotData(i).x = 100 .* newgrid;
    plotData(i).y = welfgain(1,:);
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

set(gca, 'FontSize', 32, 'TickLabelInterpreter', 'latex');
axis([0 25 -2.5 2.0]);
ylabel('Welfare gain of bailouts (percent)', 'Interpreter', 'latex', 'FontSize', 32);
xlabel('Debt/$E(y)$ ', 'Interpreter', 'latex', 'FontSize', 32);

hold on
for i = 1:numel(runs)
    scatter(plotData(i).meanDebtPct, plotData(i).meanWelfarePct,140, 'filled', 'k');
end

thetaLabels = {runs.thetaLabel};
leg1 = legend(thetaLabels{1}, thetaLabels{2}, thetaLabels{3}, 'Mean debt in simu.', ...
    'Location', 'northeast');
set(leg1, 'Interpreter', 'latex');
set(leg1, 'FontSize', 28);
legend boxoff
hold off

pngOut1 = fullfile(baseDir, 'welfare_plot_theta.png');
pngOut2 = fullfile(baseDir, 'welfgain_theta.png');
pdfOut = fullfile(baseDir, 'welfare_plot_theta.pdf');

saveas(hfig, pngOut1);
saveas(hfig, pngOut2);
print(hfig, pdfOut, '-dpdf', '-painters');

fprintf('Saved Figure C.6 outputs to:\n  %s\n  %s\n  %s\n', pngOut1, pngOut2, pdfOut);
end

function meanDebtRatio = readMeanDebtRatio(runDir, fallbackMeanDebtRatio)
resultsPath = fullfile(runDir, 'results.out');
if isfile(resultsPath)
    vals = load(resultsPath);
    if ~isempty(vals) && numel(vals) >= 13
        meanDebtRatio = vals(13) / 100;
        return
    end
end
meanDebtRatio = fallbackMeanDebtRatio;
end
