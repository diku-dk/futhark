module type sparse = {
  type csr

  module csr: {
    type mat = csr
  }
}

module sparse (T: {}) : sparse = {
  module csr = {
    type mat = bool
  }

  type csr = csr.mat
}

module spa = sparse {}
module csr = spa.csr

def main (x: csr.mat) = x
