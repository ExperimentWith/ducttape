// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

// Used by resolveNonBranchVar() in TaskTemplateBuilder
// to indicate what kind of spec we're currently resolving
class ResolveMode();
case class InputMode() extends ResolveMode;
case class ParamMode() extends ResolveMode;
case class OutputMode() extends ResolveMode;
