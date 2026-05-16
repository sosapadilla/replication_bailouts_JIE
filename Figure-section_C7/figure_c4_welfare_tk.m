function figure_c4_welfare_tk(baseDir, generatedRoot)
% FIGURE_C4_WELFARE_TK
% Reads generated Figure C.4 storage-sensitivity outputs and saves the plot.

if nargin < 1 || isempty(baseDir)
    baseDir = pwd;
end
if nargin < 2 || isempty(generatedRoot)
    generatedRoot = fullfile(baseDir, 'figure_c4_tk_runs');
end

cases = struct( ...
    'folder', {'01_tk_0.64', '02_tk_0.74', '03_tk_0.84'}, ...
    'label',  {'$\Gamma_k = 0.64$', '$\Gamma_k = 0.74$', '$\Gamma_k = 0.84$'}, ...
    'style',  {'--', '-.', ':'}, ...
    'color',  {[1 0 0], [0 0 1], [0 0.6 0]});

for i = 1:numel(cases)
    bailoutDir = fullfile(generatedRoot, cases(i).folder, 'final_bailout');
    assert(exist(bailoutDir, 'dir') == 7, 'Missing generated bailout folder: %s', bailoutDir);
    cases(i).series = readCaseSeries(bailoutDir); 
end

hfig = figure;
set(hfig, 'Color', 'w');
H = gobjects(1, numel(cases));

for i = 1:numel(cases)
    s = cases(i).series;
    H(i) = plot(s.debt_grid_pct, s.welfare_curve_pct, ...
        'LineStyle', cases(i).style, ...
        'LineWidth', 4.5, ...
        'Color', cases(i).color);
    hold on
end

plot([0 25], [0 0], 'Color', [0.85 0.85 0.85], 'LineWidth', 2.0);
dotHandle = gobjects(1,1);
for i = 1:numel(cases)
    s = cases(i).series;
    h = scatter(s.mean_debt_pct, s.dot_welfare_pct, 140, 'k', 'filled');
    if i == 1
        dotHandle = h;
    end
end
hold off

set(gca, 'FontSize', 26, 'TickLabelInterpreter', 'latex');
axis([0 25 -2.4 4.4]);
xticks([0 5 10 15 20 25]);
yticks([-2 -1 0 1 2 3 4]);
xlabel('Debt/$E(y)$ (percent)', 'Interpreter', 'latex', 'FontSize', 28);
ylabel('Welfare gain of bailouts (percent)', 'Interpreter', 'latex', 'FontSize', 28);
%title('Figure C.4: Welfare gains for different $\Gamma_k$ values', 'Interpreter', 'latex', 'FontSize', 18);
leg1 = legend([H dotHandle], cases(1).label, cases(2).label, cases(3).label, 'Mean debt in simu.', ...
    'Location', 'northeast');
set(leg1, 'Interpreter', 'latex');
set(leg1, 'FontSize', 22);
legend boxoff

pngOut = fullfile(baseDir, 'Figure_C4_welfare_Tk.png');
pdfOut = fullfile(baseDir, 'Figure_C4_welfare_Tk.pdf');
legacyOut = fullfile(baseDir, 'welfare_plot_tk.png');
saveas(hfig, pngOut);
saveas(hfig, pdfOut);
saveas(hfig, legacyOut);

fprintf('\nFigure C.4 outputs written to:\n  %s\n  %s\n  %s\n', pngOut, pdfOut, legacyOut);
end

function s = readCaseSeries(bailoutDir)
resultsPath = fullfile(bailoutDir, 'results.out');
storagePath = fullfile(bailoutDir, 'results_storage.out');
bGridPath = fullfile(bailoutDir, 'graphs_b_grid_dss.txt');

assert(isfile(resultsPath), 'Missing %s', resultsPath);
assert(isfile(storagePath), 'Missing %s', storagePath);
assert(isfile(bGridPath), 'Missing %s', bGridPath);

resNums = sscanf(strtrim(fileread(resultsPath)), '%f').';
storNums = sscanf(strtrim(fileread(storagePath)), '%f').';
bGrid = load(bGridPath);

assert(numel(resNums) >= 35, 'Unexpected results.out format in %s. Found only %d numbers.', resultsPath, numel(resNums));
assert(numel(storNums) >= 27, 'Unexpected results_storage.out format in %s. Found only %d numbers.', storagePath, numel(storNums));
assert(numel(bGrid) >= 20, 'Need at least 20 debt-grid points in %s', bGridPath);

avgY = storNums(26);
meanDebtPct = storNums(12);
welfareCurve = resNums(16:35);
debtGridPct = 100 * bGrid(1:20)' / avgY;

dotWelfare = interp1(debtGridPct, welfareCurve, meanDebtPct, 'spline');
if ~isfinite(dotWelfare)
    dotWelfare = interp1(debtGridPct, welfareCurve, meanDebtPct, 'linear', 'extrap');
end

s = struct();
s.debt_grid_pct = debtGridPct;
s.welfare_curve_pct = welfareCurve;
s.mean_debt_pct = meanDebtPct;
s.dot_welfare_pct = dotWelfare;
s.avg_y = avgY;
end
