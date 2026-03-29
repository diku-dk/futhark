// Start of server.js

class Server {

  constructor(ctx) {
    this.ctx = ctx;
    this._vars = {};
    this._types = {};
    this._commands = [ 'inputs',
                       'outputs',
                       'call',
                       'restore',
                       'store',
                       'free',
                       'clear',
                       'pause_profiling',
                       'unpause_profiling',
                       'report',
                       'rename',
                       'types',
                       'fields',
                       'project'
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
    if (entry in this.ctx.get_entry_points()) {
      return this.ctx.get_entry_points()[entry];
    } else {
      throw "Unkown entry point: " + entry;
    }
  }

  _check_var(vname) {
    if (!(vname in this._vars)) {
      throw 'Unknown variable: ' + vname;
    }
  }

  _set_var(vname, v, t) {
    this._vars[vname] = v;
    this._types[vname] = t;
  }

  _get_type(vname) {
    this._check_var(vname);
    return this._types[vname];
  }

  _get_var(vname) {
    this._check_var(vname);
    return this._vars[vname];
  }

  _cmd_inputs(args) {
    var entry = this._get_arg(args, 0);
    var inputs = this._get_entry_point(entry)[1];
    for (var i = 0; i < inputs.length; i++) {
      console.log(inputs[i]);
    }
  }

  _cmd_outputs(args) {
    var entry = this._get_arg(args, 0);
    var outputs = this._get_entry_point(entry)[2];
    for (var i = 0; i < outputs.length; i++) {
      console.log(outputs[i]);
    }
  }

  _cmd_dummy(args) {
    // pass
  }

  _cmd_free(args) {
    for (var i = 0; i < args.length; i++) {
      var vname = args[i];
      this._check_var(vname);
      delete this._vars[vname];
    }
  }

  _cmd_rename(args) {
    var oldname = this._get_arg(args, 0)
    var newname = this._get_arg(args, 1)
    if (newname in this._vars) {
      throw "Variable already exists: " + newname;
    }
    this._vars[newname] = this._vars[oldname];
    this._types[newname] = this._types[oldname];
    delete this._vars[oldname];
    delete this._types[oldname];
  }

  _cmd_types(args) {
    var types = this.ctx.get_types();
    for (var t in types) {
      console.log(t);
    }
  }

  _cmd_fields(args) {
    var type_name = this._get_arg(args, 0);
    var types = this.ctx.get_types();
    var type_info = types[type_name];
    if (!type_info || type_info[0] !== "record") {
      throw "Not a record type: " + type_name;
    }
    var fields = type_info[1];
    for (var i = 0; i < fields.length; i++) {
      console.log(fields[i][0] + " " + fields[i][1]);
    }
  }

  _cmd_project(args) {
    var to_name = this._get_arg(args, 0);
    var from_name = this._get_arg(args, 1);
    var field_name = this._get_arg(args, 2);

    if (to_name in this._vars) {
      throw "Variable already exists: " + to_name;
    }

    var from_val = this._get_var(from_name);
    var from_type = this._get_type(from_name);

    var types = this.ctx.get_types();
    var type_info = types[from_type];
    if (!type_info || type_info[0] !== "record") {
      throw "Not a record type: " + from_type;
    }

    var fields = type_info[1];
    var field_info = null;
    for (var i = 0; i < fields.length; i++) {
      if (fields[i][0] === field_name) {
        field_info = fields[i];
        break;
      }
    }

    if (field_info === null) {
      throw "No such field: " + field_name;
    }

    var field_type = field_info[1];
    var project_fn = field_info[2];
    var result = this.ctx[project_fn](from_val);
    this._set_var(to_name, result, field_type);
  }

  _cmd_call(args) {
    var entry = this._get_entry_point(this._get_arg(args, 0));
    var num_ins = entry[1].length;
    var num_outs = entry[2].length;
    var expected_len = 1 + num_outs + num_ins

    if (args.length != expected_len) {
      throw "Invalid argument count, expected " + expected_len
    }

    var out_vnames = args.slice(1, num_outs+1)
    for (var i = 0; i < out_vnames.length; i++) {
      var out_vname = out_vnames[i];
      if (out_vname in this._vars) {
        throw "Variable already exists: " + out_vname;
      }
    }
    var in_vnames = args.slice(1+num_outs);
    var ins = [];
    for (var i = 0; i < in_vnames.length; i++) {
      ins.push(this._get_var(in_vnames[i]));
    }
    // Call entry point function from string name
    var bef = performance.now()*1000;
    var vals = this.ctx[entry[0]].apply(this.ctx, ins);
    var aft = performance.now()*1000;
    if (num_outs == 1) {
      this._set_var(out_vnames[0], vals, entry[2][0]);
    } else {
      for (var i = 0; i < out_vnames.length; i++) {
        this._set_var(out_vnames[i], vals[i], entry[2][i]);
      }
    }
    console.log("runtime: " + Math.round(aft-bef));
  }

  _cmd_store(args) {
    var fname = this._get_arg(args, 0);
    for (var i = 1; i < args.length; i++) {
      var vname = args[i];
      var value = this._get_var(vname);
      var typ = this._get_type(vname);
      var fs = require("fs");
      var bin_val = construct_binary_value(value, typ);
      fs.appendFileSync(fname, bin_val, 'binary')
    }
  }

  fut_to_dim_typ(typ) {
    var type = typ;
    var count = 0;
    while (type.substr(0, 2) == '[]') {
      count = count + 1;
      type = type.slice(2);
    }
    return [count, type];
  }

  _cmd_restore(args) {
    if (args.length % 2 == 0) {
      throw "Invalid argument count";
    }

    var fname = args[0];
    var args = args.slice(1);

    var as = args;
    var reader = new Reader(fname);
    while (as.length != 0) {
      var vname = as[0];
      var typename = as[1];
      as = as.slice(2);

      if (vname in this._vars) {
        throw "Variable already exists: " + vname;
      }
      try {
        var value = read_value(typename, reader);
        if (typeof value == 'number' || typeof value == 'bigint') {
          this._set_var(vname, value, typename);
        } else {
          // We are working with an array and need to create to convert [shape, arr] to futhark ptr
          var shape= value[0];
          var arr = value[1];
          var dimtyp = this.fut_to_dim_typ(typename);
          var dim = dimtyp[0];
          var typ = dimtyp[1];
          var arg_list = [arr, ...shape];
          var fnam = "new_" + typ + "_" + dim + "d";
          var ptr = this.ctx[fnam].apply(this.ctx, arg_list);
          this._set_var(vname, ptr, typename);
        }
      } catch (err) {
        var err_msg = "Failed to restore variable " + vname + ".\nPossibly malformed data in " + fname + ".\n" + err.toString();
        throw err_msg;
      }
    }
    skip_spaces(reader);
    if (reader.get_buff().length != 0) {
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
      if (this._commands.includes(cmd)) {
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
        case 'rename': this._cmd_rename(args); break
        case 'types': this._cmd_types(args); break
        case 'fields': this._cmd_fields(args); break
        case 'project': this._cmd_project(args); break
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
        console.log('%%% OK');
      } catch (err) {
        console.log('%%% FAILURE');
        console.log(err);
        console.log('%%% OK');
      }
    }).on('close', () => { process.exit(0); });
  }
}

// End of server.js
