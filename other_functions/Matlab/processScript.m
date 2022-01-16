exptName = 'Experiment2';
raw_data = 'data/raw_data/';
processed_data = 'data/processed_data/';

file_pattern = 'eventBuffer*.mat';
gazeCols = [9, 22, 10, 23];
screenBounds = [-0.05, 1.05];

warnStr_noEvents = '%s - Experiment not found in events, skipping file\n';
warnStr_eventsMismatch = '%s - Number of onset and offset events do not match, skipping file\n';
warnStr_missingIndex = '%s - Trial: %d - Cannot find index for either start or end time, skipping trial\n';
warnStr_incompatibleBuffs = '%s - Lengths of time and main buffer do not match, skipping file\n';

fid = fopen(sprintf('data/processingLog_%s', datestr(now, 30)),'w+');
files = dir(fullfile(raw_data, file_pattern));

for file_n = 1:length(files)
    
    part_ID = regexp(files(file_n).name, '([aA-zZ]{1}[0-9]{2,8}.*)(?=\.)', 'match', 'once');
    
    file_out = fullfile(processed_data, [part_ID, '.csv']);
    
    evBuff_path = fullfile(files(file_n).folder, files(file_n).name);
    mainBuff_path = strrep(evBuff_path, 'event', 'main');
    timeBuff_path = strrep(evBuff_path, 'event', 'time');
    
    load(evBuff_path)
    
    exptN = contains(eventBuffer(:,3), exptName);
    
    if ~any(exptN)
        msg = sprintf(warnStr_noEvents, files(file_n).name);
        warning(msg)
        fprintf(fid, msg);
        continue
    end
    
    stimOnset = contains(eventBuffer(:,3), 'StimulusOnset');
    stimOffset = contains(eventBuffer(:,3), 'StimulusOffset');
    
    specificOnsets = eventBuffer(exptN & stimOnset, :);
    specificOffsets = eventBuffer(exptN & stimOffset, :);
    
    if length(specificOnsets) ~= length(specificOnsets)
        msg = sprintf(warnStr_eventsMismatch, files(file_n).name);
        warning(msg)
        fprintf(fid, msg);
        continue
    end
    
    load(mainBuff_path)
    load(timeBuff_path)

    if length(timeBuffer) ~= length(mainBuffer)
        msg = sprintf(warnStr_incompatibleBuffs, files(file_n).name);
        warning(msg)
        fprintf(fid, msg);
        continue
    end
        
    allData = [double(timeBuffer) mainBuffer];
    
    clear *Buffer
    
    participantData = table();
    
    for event_n = 1:length(specificOnsets)
        
        windowStart = specificOnsets{event_n, 2};
        windowEnd = specificOffsets{event_n, 2};
        
        winStartIdx = find(allData > windowStart, 1);
        winEndIdx = find(allData > windowEnd, 1);
        
        if isempty(winStartIdx) || isempty(winEndIdx)
            msg = sprintf(warnStr_missingIndex, files(file_n).name, event_n);
            warning(msg)
            fprintf(fid, msg);
            continue
        end
        
        LXRX_LYRY = allData(winStartIdx:winEndIdx,gazeCols);
        LXRX_LYRY(LXRX_LYRY < screenBounds(1) | LXRX_LYRY > screenBounds(2)) = NaN;
        epoch_time = (allData(winStartIdx:winEndIdx, 1)-allData(winStartIdx,1))/1e6;
        eventData = [allData(winStartIdx:winEndIdx, 1) epoch_time LXRX_LYRY];
                  
        eventData = array2table(eventData, 'VariableNames',...
            {'abs_time', 'epoch_time', 'LX', 'RX', 'LY' 'RY'});
        
        eventData.trial_ID(:) = event_n;
        
        participantData = [participantData; eventData];
    end
    participantData.part_ID(:) = {part_ID};
    writetable(participantData, file_out, 'FileType', 'text')
end
fclose(fid);