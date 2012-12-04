Summary: mruby IIJ fork
Name: mruby-iij
Version: 0.0
Release: 0
Source0: iij
License: MIT
Vendor: Internet Initiative Japan Inc.
Group: Development/Languages
Packager: Tomoyuki Sahara (tsahara@iij.ad.jp)
Buildroot: %{_tmppath}/%{name}-%{version}-%{release}-root
URL: http://github.com/iij/mruby/
BuildRequires: make, gcc, bison
Requires: openssl-devel

%description
Lightweight Ruby, with Unix-dependent extensions.

%prep
%setup -T -c -n mruby-iij
cd ..
unzip -q %{SOURCE0}

%build
make

%install
install -d $RPM_BUILD_ROOT/%{_bindir}
install -c -s -m755 bin/mirb $RPM_BUILD_ROOT/%{_bindir}
install -c -s -m755 bin/mrbc $RPM_BUILD_ROOT/%{_bindir}
install -c -s -m755 bin/mruby $RPM_BUILD_ROOT/%{_bindir}
install -d $RPM_BUILD_ROOT/%{_libdir}/mruby
install -c -m644 ext/* $RPM_BUILD_ROOT/%{_libdir}/mruby

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%{_bindir}/mirb
%{_bindir}/mrbc
%{_bindir}/mruby
%dir %{_libdir}/mruby
%{_libdir}/mruby/*

%changelog
* Thu Nov 15 2012 Tomoyuki Sahara <tsahara@iij.ad.jp> 0.0-0
initial RPM package.
