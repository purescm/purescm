#!/usr/bin/env node

import { spawnSync } from 'child_process';
let env = process.env;
env['DYLD_LIBRARY_PATH'] = env['CHEZ_DYLD_LIBRARY_PATH'];
spawnSync('scheme', ["--libdirs", "lib:", "--program", "test-runtime.ss"], { env, stdio: 'inherit' });
