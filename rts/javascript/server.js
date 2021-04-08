// Start of server.js

class Server {

    
  constructor(ctx) {
    this.ctx = ctx;
    this._vars = {};
    this._commands = [ 'inputs',
                'outputs',
                'call',
                'restore',
                'store',
                'free',
                'clear',
                'pause_profiling',
                'unpause_profiling',
                'report'
               ];
  }

  _get_arg(args, i) {
    if (i < args.length) {
      return args[i];
    } else {
      throw 'Insufficient command args';
    }
  }
  
  _get_entry_point(entry) {
    // TODO Context contains a list of entry points, my implementation does not
    // Seems to be a dictionary
    if (this.ctx.entry_points.includes(entry)) {
      return this.entry_points[entry];
    } else {
      throw "Unkown entry point: " + entry;
    }
  }


  _check_var(vname) {
    if (vname in this._vars) {
      throw 'Unknown variable: ' + vname;
    }
  }

  _get_var(vname) {
    this._check_var(vname);
    return this._vars[vname];
  }

  _cmd_inputs(args) {
    var entry = self._get_arg(args, 0);
    var inputs = _get_entry_point(entry)[0];
    for (var i = 0; i < e.length; i++) {
      console.log(e[i]);
    }
  }

  _cmd_inputs(args) {
    var entry = self._get_arg(args, 0);
    var outputs = _get_entry_point(entry)[1];
    for (var i = 0; i < e.length; i++) {
      console.log(e[i]);
    }
  }

  _cmd_dummy(args) {
    // pass
  }

  _cmd_free(args) {
    for (var i = 0; i < args.length; i++) {
      var vname = args[i];
      _check_var(vname);
      const idx = _vars.getIndex(vname);
      _vars.splice(idx, 1);
    }
  }

  _cmd_call(args) {
    var entry = _get_entry_point(get_arg(args, 0));
    var num_ins = entry[0].length;
    var num_outs = entry[0].length;
    var expected_len = 1 + num_outs + num_ins

    if (args.length != expected_len) {
      throw "Invalid argument count, expected " + expected_len
    }

    var out_vnames = args.slice(1, num_outs+1)
    for (var i = 0; i < out_vnames.length; i++) {
      var out_vname = out_vnames[i];
      if (this._vars.includes(out_vname)) {
        throw "Variable already exists: " + out_vname;
      }
    }
    var in_vnames = args.slice(1+num+outs);
    var ins = [];
    for (var i = 0; i < in_vnames.length; i++) {
      ins.push(_get_var(in_vnames[i]));
    }
    // TODO Figure this bad boy out
    try {
      var runtime = getattr(self._ctx, args[0]);
      var vals;
    } 
    catch (err) {
      throw "" + err;
    }

    // This probablty isn't right
    console.log('runtime: ' + runtime);
    if (num_outs == 1) {
      this._vars[out_vnames[0]] = vals;
    } else {
      for (var i = 0; i < out_vnames.length; i++) {
        this._vars[out_vnames[i]] = vals[i];
      }
    }
  }


  _cmd_store(args) {
    var fname = _get_arg(args, 0);
    for (var i = 1; i < args.length; i++) {
      var vname = args[i];
      var value = _get_var(vname);
      //TODO right this more elengantly
      //TODO make sure file is open in binary mode
      var fs = require("fs");
      var bin_val = construct_binary_value(this._vars[vname]);
      console.log("What does bin val look like??");
      console.log(bin_val);
      fs.appendFile(fname, bin_val);
    }
  }

  _cmd_restore(args) {
    if (args.length % 2 == 0) {
      throw "Invalid argument count";
    }

    var fname = args[0];
    // TODO make sure this is legal 
    var args = args.slice(1);
    
    // Reading from file is part of values.js
    var reader = new Reader(fname);
    while (args != []) {
      var vname = args[0];
      var typename = args[1];
      args = args.slice(2);
      
      // TODO finish the rest of this function
      if (this._vars.includes(vname)) {
        throw "Variable already exists: " + vname;
      }
      try {
        this._vars[vname] = read_value(typename, reader);
      } catch (err) {
        err_msg = "Failed to restore variable " + vname + ".\nPossibly malformed data in " + fname + ".\n";
        throw "Failed to restore variable " + err_msg;
      }
    }
    skip_spaces(reader);
    if (reader.get_buff() == "") {
      throw "Expected EOF after reading values";
    }
  }




  _process_line(line) {
    // TODO make sure it splits on anywhite space
    var words = line.split(" ");
    if (words.length == 0) {
      throw "Empty line";
    } else {
      var cmd = words[0];
      var args = words.splice(1);
      if (cmd in this._commands) {
        switch (cmd) {
          case 'inputs': this._cmd_inputs(args); break;
          case 'outputs': this._cmd_outputs(args); break
          case 'call': this._cmd_call(args); break
          case 'restore': this._cmd_restore(args); break
          case 'store': this._cmd_store(args); break
          case 'free': this._cmd_free(args); break
          case 'clear': this._cmd_dummy(args); break
          case 'pause_profiling': this._cmd_dummy(args); break
          case 'unpause_profiling': this._cmd_dummy(args); break
          case 'report': this._cmd_dummy(args); break
       }
      } else {
        throw "Unknown command: " + cmd;
      }
    }
  }

  run() {
    console.log('%%% OK'); // TODO figure out if flushing is neccesary for JS
    const readline = require('readline');
    const rl = readline.createInterface(process.stdin);
    rl.on('line', (line) => {
      if (line == "") {
        rl.close();
      }
      try {
        this._process_line(line);
      } catch (err) {
        console.log('%%% FAILURE');
        console.log(err);
      }
    });
  }
}
