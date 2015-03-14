# Target:  Motorola 88000 using coff.
# Avoid a huge BFD library
TDEFAULTS=-DSELECT_ARCHITECTURES=bfd_m88k_arch -DSELECT_VECS='&m88kbcs_vec'

