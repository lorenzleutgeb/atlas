# This file is generated by gradle2nix.
#
# Example usage (e.g. in default.nix):
#
#     with (import <nixpkgs> {});
#     let
#       buildGradle = callPackage ./gradle-env.nix {};
#     in
#       buildGradle {
#         envSpec = ./gradle-env.json;
#
#         src = ./.;
#
#         gradleFlags = [ "installDist" ];
#
#         installPhase = ''
#           mkdir -p $out
#           cp -r app/build/install/myproject $out
#         '';
#       }

{ stdenv, buildEnv, fetchurl, gradleGen, writeText, writeTextDir }:

{ envSpec
, pname ? null
, version ? null
, enableParallelBuilding ? true
, gradleFlags ? [ "build" ]
, gradlePackage ? null
, enableDebug ? false
, ... } @ args:

let
  inherit (builtins)
    filter sort replaceStrings attrValues match fromJSON
    concatStringsSep;

  inherit (stdenv.lib)
    versionOlder unique mapAttrs last concatMapStringsSep removeSuffix
    optionalString groupBy' readFile hasSuffix;

  mkDep = depSpec: stdenv.mkDerivation {
    inherit (depSpec) name;

    src = fetchurl {
      inherit (depSpec) urls sha256;
    };

    phases = "installPhase";

    installPhase = ''
      mkdir -p $out/${depSpec.path}
      ln -s $src $out/${depSpec.path}/${depSpec.name}
    '';
  };

  mkModuleMetadata = deps:
    let
      ids = filter
        (id: id.type == "pom")
        (map (dep: dep.id) deps);

      modules = groupBy'
        (meta: id:
          let
            isNewer = versionOlder meta.latest id.version;
            isNewerRelease =
              !(hasSuffix "-SNAPSHOT" id.version) &&
              versionOlder meta.release id.version;
          in {
            groupId = id.group;
            artifactId = id.name;
            latest = if isNewer then id.version else meta.latest;
            release = if isNewerRelease then id.version else meta.release;
            versions = meta.versions ++ [id.version];
          }
        )
        {
          latest = "";
          release = "";
          versions = [];
        }
        (id: "${replaceStrings ["."] ["/"] id.group}/${id.name}/maven-metadata.xml")
        ids;

    in
      attrValues (mapAttrs (path: meta:
        let
          versions' = sort versionOlder (unique meta.versions);
        in
          with meta; writeTextDir path ''
            <?xml version="1.0" encoding="UTF-8"?>
            <metadata modelVersion="1.1">
              <groupId>${groupId}</groupId>
              <artifactId>${artifactId}</artifactId>
              <versioning>
                ${optionalString (latest != "") "<latest>${latest}</latest>"}
                ${optionalString (release != "") "<release>${release}</release>"}
                <versions>
                  ${concatMapStringsSep "\n    " (v: "<version>${v}</version>") versions'}
                </versions>
              </versioning>
            </metadata>
          ''
      ) modules);

  mkSnapshotMetadata = deps:
    let
      snapshotDeps = filter (dep: dep ? build && dep ? timestamp) deps;

      modules = groupBy'
        (meta: dep:
          let
            id = dep.id;
            isNewer = dep.build > meta.buildNumber;
            # Timestamp values can be bogus, e.g. jitpack.io
            updated = if (match "[0-9]{8}\.[0-9]{6}" dep.timestamp) != null
                      then replaceStrings ["."] [""] dep.timestamp
                      else "";
          in {
            groupId = id.group;
            artifactId = id.name;
            version = id.version;
            timestamp = if isNewer then dep.timestamp else meta.timestamp;
            buildNumber = if isNewer then dep.build else meta.buildNumber;
            lastUpdated = if isNewer then updated else meta.lastUpdated;
            versions = meta.versions or [] ++ [{
              classifier = id.classifier or "";
              extension = id.extension;
              value = "${removeSuffix "-SNAPSHOT" id.version}-${dep.timestamp}-${toString dep.build}";
              updated = updated;
            }];
          }
        )
        {
          timestamp = "";
          buildNumber = -1;
          lastUpdated = "";
        }
        (dep: "${replaceStrings ["."] ["/"] dep.id.group}/${dep.id.name}/${dep.id.version}/maven-metadata.xml")
        snapshotDeps;

      mkSnapshotVersion = version: ''
        <snapshotVersion>
          ${optionalString (version.classifier != "") "<classifier>${version.classifier}</classifier>"}
          <extension>${version.extension}</extension>
          <value>${version.value}</value>
          ${optionalString (version.updated != "") "<updated>${version.updated}</updated>"}
        </snapshotVersion>
      '';

    in
      attrValues (mapAttrs (path: meta:
        with meta; writeTextDir path ''
          <?xml version="1.0" encoding="UTF-8"?>
          <metadata modelVersion="1.1">
            <groupId>${groupId}</groupId>
            <artifactId>${artifactId}</artifactId>
            <version>${version}</version>
            <versioning>
              <snapshot>
                ${optionalString (timestamp != "") "<timestamp>${timestamp}</timestamp>"}
                ${optionalString (buildNumber != -1) "<buildNumber>${toString buildNumber}</buildNumber>"}
              </snapshot>
              ${optionalString (lastUpdated != "") "<lastUpdated>${lastUpdated}</lastUpdated>"}
              <snapshotVersions>
                ${concatMapStringsSep "\n    " mkSnapshotVersion versions}
              </snapshotVersions>
            </versioning>
          </metadata>
        ''
      ) modules);

  mkRepo = project: type: deps: buildEnv {
    name = "${project}-gradle-${type}-env";
    paths = map mkDep deps ++ mkModuleMetadata deps ++ mkSnapshotMetadata deps;
  };

  mkInitScript = projectSpec:
    let
      repos = mapAttrs (mkRepo projectSpec.name) projectSpec.dependencies;
    in
      writeText "init.gradle" ''
        static def offlineRepo(RepositoryHandler repositories, String env, String path) {
            repositories.clear()
            repositories.maven {
                name "Nix''${env.capitalize()}MavenOffline"
                url path
                metadataSources {
                    it.gradleMetadata()
                    it.mavenPom()
                    it.artifact()
                }
            }
            repositories.ivy {
                name "Nix''${env.capitalize()}IvyOffline"
                url path
                layout "maven"
                metadataSources {
                    it.gradleMetadata()
                    it.ivyDescriptor()
                    it.artifact()
                }
            }
        }

        gradle.settingsEvaluated {
            offlineRepo(it.pluginManagement.repositories, "plugin", "${repos.plugin}")
        }

        gradle.projectsLoaded {
            allprojects {
                buildscript {
                    offlineRepo(repositories, "buildscript", "${repos.buildscript}")
                }
                offlineRepo(repositories, "project", "${repos.project}")
            }
        }
      '';

  mkGradle = gradleSpec:
    gradleGen.gradleGen {
      inherit (gradleSpec) nativeVersion;

      name = "gradle-${gradleSpec.version}-${gradleSpec.type}";

      src = fetchurl {
        inherit (gradleSpec) url sha256;
      };
    };

  mkProjectEnv = projectSpec: {
    inherit (projectSpec) name version;
    initScript = mkInitScript projectSpec;
    gradle = args.gradlePackage or mkGradle projectSpec.gradle;
  };

  gradleEnv = mapAttrs
    (_: p: mkProjectEnv p)
    (fromJSON (readFile envSpec));

  projectEnv = gradleEnv."";
  pname = args.pname or projectEnv.name;
  version = args.version or projectEnv.version;

in stdenv.mkDerivation (args // {

  inherit pname version;

  nativeBuildInputs = (args.nativeBuildInputs or []) ++ [ projectEnv.gradle ];

  buildPhase = args.buildPhase or ''
    runHook preBuild

    (
    set -x

    # use the init script here
    TMPHOME=$(mktemp -d)
    mkdir -p $TMPHOME/init.d
    cp ${projectEnv.initScript} $TMPHOME/init.d

    env \
      "GRADLE_USER_HOME=$TMPHOME" \
      gradle --offline --no-daemon --no-build-cache \
        --info --full-stacktrace --warning-mode=all \
        ${optionalString enableParallelBuilding "--parallel"} \
        ${optionalString enableDebug "-Dorg.gradle.debug=true"} \
        ${concatStringsSep " " gradleFlags}
    )

    runHook postBuild
  '';

  dontStrip = true;
})
