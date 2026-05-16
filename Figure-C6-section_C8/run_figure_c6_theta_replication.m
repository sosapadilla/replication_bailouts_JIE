function run_figure_c6_theta_replication(baseDir)
%RUN_FIGURE_C6_THETA_REPLICATION Build the theta comparison data and plot.
%   Workflow for each theta folder:
%   1. Compile/run the no-bailout Fortran variant.
%   2. Save the required no-bailout outputs into nb_files.
%   3. Compile/run the bailout Fortran variant.
%   4. After all theta cases finish, generate Figure C.6.

clc;
fprintf('\n=== Figure C.6 theta replication runner ===\n');
setenv('PATH', ['/opt/homebrew/bin:' getenv('PATH')]);

if nargin < 1 || isempty(baseDir)
    fullPath = mfilename('fullpath');
    if isempty(fullPath)
        baseDir = pwd;
    else
        baseDir = fileparts(fullPath);
    end
end

mainDir = baseDir;
thetaFolders = { ...
    'bailouts-default-theta-0.4', ...
    'bailouts-default-theta-0.5', ...
    'bailouts-default-theta-0.6'};

for i = 1:numel(thetaFolders)
    runDir = fullfile(mainDir, thetaFolders{i});
    fprintf('\n==============================\n');
    fprintf('Processing %s\n', thetaFolders{i});
    fprintf('==============================\n');

    nobailSrc = fullfile(runDir, 'bailouts_optimal_nobailout.f90');
    bailoutSrc = fullfile(runDir, 'bailouts_optimal_bailout.f90');
    assert(isfile(nobailSrc), 'Missing source: %s', nobailSrc);
    assert(isfile(bailoutSrc), 'Missing source: %s', bailoutSrc);

    nobailExe = 'bailouts_optimal_nobailout.out';
    bailoutExe = 'bailouts_optimal_bailout.out';

    fprintf('Compiling no-bailout code...\n');
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', nobailSrc, nobailExe), runDir, ...
        sprintf('%s no-bailout compilation failed.', thetaFolders{i}));

    fprintf('Running no-bailout code...\n');
    runShell(sprintf('./%s', nobailExe), runDir, ...
        sprintf('%s no-bailout run failed.', thetaFolders{i}));

    snapshotNoBailOutputs(runDir, fullfile(runDir, 'nb_files'));

    fprintf('Compiling bailout code...\n');
    runShell(sprintf('gfortran -O3 "%s" -o "%s"', bailoutSrc, bailoutExe), runDir, ...
        sprintf('%s bailout compilation failed.', thetaFolders{i}));

    fprintf('Running bailout code...\n');
    runShell(sprintf('./%s', bailoutExe), runDir, ...
        sprintf('%s bailout run failed.', thetaFolders{i}));
end

figure_c6_welfare_theta(baseDir);
end

function snapshotNoBailOutputs(runDir, nbDir)
if ~exist(nbDir, 'dir')
    mkdir(nbDir);
else
    clearFolder(nbDir);
end

nbFiles = { ...
    'graphs_default_dss.txt', ...
    'graphs_b_next.txt', ...
    'graphs_labor.txt', ...
    'graphs_consumption.txt', ...
    'graphs_cons_bank.txt'};

for k = 1:numel(nbFiles)
    src = fullfile(runDir, nbFiles{k});
    dst = fullfile(nbDir, nbFiles{k});
    assert(isfile(src), 'Missing no-bailout benchmark file: %s', src);
    copyfile(src, dst);
end
end

function clearFolder(folderPath)
contents = dir(folderPath);
for i = 1:numel(contents)
    name = contents(i).name;
    if strcmp(name, '.') || strcmp(name, '..')
        continue
    end
    target = fullfile(folderPath, name);
    if contents(i).isdir
        rmdir(target, 's');
    else
        delete(target);
    end
end
end

function runShell(cmd, workDir, failMsg)
oldDir = pwd;
cleanup = onCleanup(@() cd(oldDir));
cd(workDir);
[status, out] = system(cmd);
if status ~= 0
    error('%s\nCommand: %s\nOutput:\n%s', failMsg, cmd, out);
end
end
